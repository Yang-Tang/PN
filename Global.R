library(shiny)
library(DT)
library(rhandsontable)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)


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

# init matrix for conc_init and vol_init of source plate
INIT_MATRIX <- matrix(rep(as.numeric(NA), times=96), nrow=8) %>% 
  set_rownames(LETTERS[1:8]) %>% 
  set_colnames(1:12)

# Helper functions --------------------------------------------------------

# read conc data of source plate and convert to a table matrix
readData <- function(file){
  file %>% 
    read.csv() %>% 
    mutate(Row=str_extract(Well, '^[A-Z]'),
           Col=as.numeric(str_extract(Well, '\\d+$'))) %>% 
    select(Row, Col, Value) %>% 
    right_join(expand.grid(Row=LETTERS[1:8], Col=1:12)) %>% 
    spread(Col, Value) %>% 
    select(-Row) %>% 
    as.matrix() %>% 
    set_rownames(LETTERS[1:8])
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


# split a large volumn into smaller volumns to match vol_max and vol_min of LiHa
splitVol <- function(vol, vol_max, vol_min){
  if(vol < vol_min) return(list())
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
}


# build workplan from a data frame with position, conc_init, vol_init columns
buildWorkplan <- function(data, vol_dead, vol_min, vol_max){
  if(nrow(data)<2) return()
  conc_final <- findConcFinal(data$conc_init, data$vol_init, vol_dead, vol_min)
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
buildWorklist <- function(workplan, vol_max, vol_min){
  workplan %>% 
    transmute(`Source Labware Lable` = 'Source1',
              `Source Position` =  position,
              `Destination Labware Lable` = 'Dest1',
              `Destination Position` = position,
              Volumn = vol_source) %>% 
    mutate(Volumn=splitVol(volumn, vol_max, vol_min)) %>% 
    unnest(Volumn) %>% 
    arrange(`Destination Position`)
}


# build worklist1 from workplan for diluent aspiration
buildWorklist1 <- function(workplan, vol_max, vol_min){
  workplan %>% 
    transmute(`Source Labware Lable` = 'Source',
              `Source Position` =  position,
              `Destination Labware Lable` = 'Dest1',
              `Destination Position` = position,
              Volumn = vol_source) %>% 
    mutate(Volumn=splitVol(volumn, vol_max, vol_min)) %>% 
    unnest(Volumn) %>% 
    arrange(`Destination Position`) %>% 
    mutate(`Source Position` = rep(1:8, length.out=n()))
}

# start EOVware and run workplan
runWorkplan <- function(worklist, worklist1, path, EOVware_path, user_name, password, script_path){
  
  # output Worklist.csv and Worklist1.csv
  write.csv(worklist, file=file.path(path, 'Worklist.csv'), row.names=F, quote=F)
  write.csv(worklist1, file=file.path(path, 'Worklist1.csv'), row.names=F, quote=F)
  
  # start EOVware and run
  cmd <- paste(EOVware_path, '-u', user_name, '-w', password, '-r', script_path)
  shell(cmd)
  
}

