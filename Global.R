library(shiny)
library(shinyjs)
library(rhandsontable)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)


# Variable def ------------------------------------------------------------

# vol_max: max volumn of LiHa tips
# vol_min: min volumn of LiHa tips
# vol_dead: dead volumn of 96-well plate
# conc_final: final concentration after normalization
# conc_init: (vector) initial concentration of liquid in 96-well plate
# position: (vector, 1-96) position index of liquid in 96-well plate
# vol_init: (vector) initial volumn of liquid in 96-well plate
# vol_source: (vector) volumn to aspirate from source plate
# vol_diluent: (vector) volumn of dilution solution to add to dest plate
# conc_threshold: the threshold to differentiate high/low concentration, which will be treated use different strategies
# "Source1": labware lable of source plate
# "Dest1": labware lable of dest plate
# "Source": lavware lable of diluent
# 8: number of LiHa tips

# load settings
settings <- readLines('settings.txt') %>% 
  str_match(':[[:blank:]]*(.+?)[[:blank:]]*#') %>% 
  .[, 2]
vol_max <- as.numeric(settings[1])
vol_min <- as.numeric(settings[2])
n_tips <- as.numeric(settings[3])
vol_dead <- as.numeric(settings[4])
source_plate <- as.character(settings[5])
dest_plate <- as.character(settings[6])
source_nrow <- as.numeric(settings[7])
source_ncol <- as.numeric(settings[8])
dest_nrow <- as.numeric(settings[9])
dest_ncol <- as.numeric(settings[10])
diluent <- as.character(settings[11])
username <- as.character(settings[12])
password <- as.character(settings[13])
script_path <- as.character(settings[14])
EVOware_path <- as.character(settings[15])



# init matrix for conc_init and vol_init of source plate
INIT_MATRIX <- matrix(rep(as.numeric(NA), times=source_nrow*source_ncol), nrow=source_nrow) %>% 
  set_rownames(LETTERS[1:source_nrow]) %>% 
  set_colnames(1:source_ncol)


# Helper functions --------------------------------------------------------

# read conc data of source plate and convert to a table matrix
readData <- function(file){
  tmpfile <- tempfile(fileext='.xls')
  file.copy(from=file, to=tmpfile)
  read_excel(tmpfile, skip=0) %>% 
    select(14) %>% 
    unlist() %>% 
    c(rep(NA, times=source_nrow*source_ncol-length(.))) %>% 
    matrix(nrow=source_nrow) %>% 
    set_rownames(LETTERS[1:source_nrow]) %>% 
    set_colnames(1:source_ncol)
}


# find the largest possable conc_final
findConcFinal <- function(conc_init, vol_init, vol_dead, vol_min){
  
  # init conc_final, start from min(conc_init)
  conc_final <- min(conc_init)
  
  # set vol_source as max by only taking off dead volumn from vol_init
  vol_source <- vol_init-vol_dead
  
  # loop to find conc_final, decreace 1 per loop
  while(conc_final>0){
    
    # calc vol_diluent using current conc_final when vol_sourse is set as max
    vol_diluent <- vol_source * (conc_init - conc_final) / conc_final
    
    # check whether current vol_diluent is reasonable
    
    # first, sort vol_diluent, find the minimal and the second minimal vol_diluent
    vol_sort <- sort(vol_diluent)
    v1 <- vol_sort[1]
    v2 <- vol_sort[2]
    
    # min(vol_diluent) should larger than vol_min 
    if(v1 >= vol_min) return(conc_final)
    
    # or the minimal vol_diluent near zero (no need to dilution) and other vol_diluent larger than vol_min
    if(v1 <= 0.1 & v2 >= vol_min) return(conc_final)
    
    # else decreace conc_final by 1 and try again
    conc_final <- conc_final - 1
    
  }
  
  return()
  
}


# calc conc_threshold
calcConThreshold <- function(conc_final, vol_max, vol_min){
  conc_final * (vol_max + vol_min) / vol_min
}


# calc vol_source from conc_final
calcVolSource <- function(conc_init, vol_init, conc_final, conc_threshold, vol_min, vol_max){
  
  mapply(function(conc_init, vol_init){
    if(conc_init >= conc_threshold) {
      # for high concentration source just aspirate the minimal volumn
      return(vol_min)
    } else {
      # for low concentration sourse
      return(min(
        vol_init - vol_min, # aspirate the largest possiable source if concentration is too low
        vol_max * conc_final / (conc_init - conc_final) # or aspirate certern amount to keep vol_sulution=vol_max
      ))
    }
  }, conc_init, vol_init)
  
}


# calc vol_diluent from vol_source
calcVolSolution <- function(vol_source, conc_init, conc_final){
  vol_source * (conc_init - conc_final) / conc_final
}


# split a vector of large volumns into smaller volumns to match vol_max and vol_min of LiHa
splitVol <- function(vols, vol_max, vol_min){
  vols %>% 
    sapply(function(vol){
      if(vol < vol_min) return(NA)
      times_max <- vol %/% vol_max
      left <- vol %% vol_max
      if(left == 0){
        out <- rep(vol_max, times=times_max)
      } else if(left < vol_min) {
        out <- c(rep(vol_max, times=times_max-1), 
                 rep((left + vol_max) / 2, times=2))
      } else {
        out <- c(rep(vol_max, times=times_max), left)
      }
      return(list(out))
    })
}


# build workplan from a data frame with position, conc_init, vol_init columns
buildWorkplan <- function(data, vol_dead, vol_min, vol_max, conc_final=NULL){
  if(nrow(data)<2) return()
  max_conc_final <- findConcFinal(data$conc_init, data$vol_init, vol_dead, vol_min)
  if(is.null(conc_final)){
    conc_final <- max_conc_final
  } else {
    conc_final <- min(conc_final, max_conc_final)
  }
  conc_threshold <- calcConThreshold(conc_final, vol_max, vol_min)
  data %>% 
    mutate(
      vol_source=calcVolSource(conc_init, vol_init, conc_final, conc_threshold, vol_min, vol_max),
      vol_diluent=calcVolSolution(vol_source, conc_init, conc_final),
      expected_final_conc=vol_source * conc_init / (vol_source + vol_diluent),
      expected_final_vol= vol_source + vol_diluent
    )
}


# build worklist from workplan for source aspiration
buildWorklist <- function(workplan, vol_max, vol_min, source_plate, dest_plate){
  workplan %>% 
    transmute(`Source Labware Lable` = source_plate,
              `Source Position` =  position,
              `Destination Labware Lable` = dest_plate,
              `Destination Position` = position,
              Volumn = vol_source) %>% 
    mutate(Volumn=splitVol(Volumn, vol_max, vol_min)) %>% 
    unnest(Volumn) %>% 
    mutate(Volumn=round(Volumn, 1)) %>% 
    arrange(`Destination Position`)
}


# build worklist1 from workplan for diluent aspiration
buildWorklist1 <- function(workplan, vol_max, vol_min, diluent, dest_plate, n_tips){
  workplan %>% 
    transmute(`Source Labware Lable` = diluent,
              `Source Position` =  position,
              `Destination Labware Lable` = dest_plate,
              `Destination Position` = position,
              Volumn = vol_diluent) %>% 
    mutate(Volumn=splitVol(Volumn, vol_max, vol_min)) %>% 
    unnest(Volumn) %>% 
    filter(!is.na(Volumn)) %>% 
    mutate(Volumn=round(Volumn, 1)) %>% 
    arrange(`Destination Position`) %>% 
    mutate(`Source Position` = rep(1:n_tips, length.out=n()))
}

# start EVOware and run workplan
runWorkplan <- function(worklist, worklist1, script_path, EVOware_path, username, password){
  
  # output Worklist.csv and Worklist1.csv
  write.csv(worklist, file=file.path(dirname(script_path), 'Worklist.csv'), row.names=F, quote=F)
  write.csv(worklist1, file=file.path(dirname(script_path), 'Worklist1.csv'), row.names=F, quote=F)
  
  # start EVOware and run
  cmd <- paste(paste0('"', EVOware_path, '"'), 
               '-u', username, '-w', password, '-b',
               '-r', script_path)
  shell(cmd)
  
}

# get pid of Evoware.exe
getPid <- function(process='Evoware.exe'){
  system2('tasklist', stdout=TRUE) %>% 
    str_match(paste0(process, '\\s+(\\d+)')) %>% 
    .[,2] %>% 
    na.omit() %>% 
    as.numeric()
}


