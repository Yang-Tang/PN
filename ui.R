shinyUI(fluidPage(
  
  #titlePanel("Plate Normalizer"),
  useShinyjs(),
  
  fluidRow(
    
    column(3),
    
    column(6,
           
           titlePanel("Plate Normalizer"),
           
           
           tabsetPanel(
             
             tabPanel(
               
               'Source plate',
               
               #hr(),
               
               h3('Concentration'),
               
               fileInput('load_conc', 'Load conc'),
               
               rHandsontableOutput('conc_tbl'),
               
               br(), hr(),
               
               h3('Volumn'),
               
               numericInput('uni_vol', 'Volumn', 50, 20, 1000, 1, width='100px'),
               
               actionButton('fill', 'Fill all'),

               br(), br(),
               
               rHandsontableOutput('vol_tbl')
               
             ),
             
             
             tabPanel(
               
               'Work plan',
               
               hr(),
               
               uiOutput('start_btn'),
               
               dataTableOutput('work_plan_DT')
               
             )
             
           ) 
           
           
    ),
    
    column(3)
    
  )
  
))
