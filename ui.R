shinyUI(fluidPage(

  titlePanel("PN"),
  
  fluidRow(
    
    column(
      
      5,
      
      fileInput('load_conc', 'Load conc'),
      
      rHandsontableOutput('conc_tbl'),
      
      br(), br(),
      
      rHandsontableOutput('vol_tbl')
      
    ),
    
    column(
      
      7,
      
      h4('Work Plan'),
      
      uiOutput('start_btn'),
      
      dataTableOutput('work_plan_DT')
      
    )
    
    
    
  )
  
  
  

))
