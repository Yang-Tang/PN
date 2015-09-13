shinyUI(fluidPage(
  
  #titlePanel("Plate Normalizer"),
  useShinyjs(),
  
  fluidRow(
    
    column(1),
    
    column(10,
           
           titlePanel("Plate Normalizer"),
           
           
           tabsetPanel(
             
             tabPanel(
               
               'Source plate',
               
               fluidRow(
                 
                 column(
                   
                   6,
                   
                   h3('Concentration'),
                   
                   rHandsontableOutput('conc_tbl'),
                   
                   fileInput('load_conc', 'Load concentration from file'),
                   
                   numericInput('user_final_conc', 'Final concentration: ', 1, 0.001, 10000, 1, width='40%'),
                   
                   checkboxInput('auto_final_conc', 'Dilute to highest possable concentration', F)
                   
                 ),
                 
                 column(
                   
                   6,
                   
                   h3('Volumn'),
                   
                   rHandsontableOutput('vol_tbl'),
                   
                   numericInput('uni_vol', 'Univesal volumn to all wells', 50, 20, 1000, 1, width='50%'),
                   
                   actionButton('fill', 'Fill all')
                   
                 )
                 
               )

             ),
             
             tabPanel(
               
               'Dest plate',
               
               br(),
               
               uiOutput('start_btn'),
               
               fluidRow(
                 
                 column(
                   
                   6,
                   
                   uiOutput('source2dest_title'),
                   
                   rHandsontableOutput('source2dest_tbl'),
                   
                   uiOutput('finalconc_title'),
                   
                   rHandsontableOutput('finalconc_tbl')
                   
                 ),
                 
                 column(
                   
                   6,
                   
                   uiOutput('diluent2dest_title'),
                   
                   rHandsontableOutput('diluent2dest_tbl'),
                   
                   uiOutput('finalvol_title'),
                   
                   rHandsontableOutput('finalvol_tbl')
                   
                 )
                 
                 
               )
               
             )
             
           ) 
           
           
    ),
    
    column(1)
    
  )
  
))
