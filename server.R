
shinyServer(function(input, output) {
  
  values <- reactiveValues(conc=INIT_MATRIX, vol=INIT_MATRIX)
  
  work_plan <- reactive({
    data_frame(position=1:96,
               conc_init=as.vector(values$conc),
               vol_init=as.vector(values$vol)) %>% 
      filter(!is.na(conc_init), !is.na(vol_init)) %>% 
      buildWorkplan(vol_dead=10, vol_min=10, vol_max=200)
  })
  
  observeEvent(work_plan(), {
    print(work_plan())
  })
  
  observeEvent(input$load_conc, {
    input$load_conc$datapath %>% 
      readData() -> values$conc
  })
  
  observeEvent(input$conc_tbl, {
    input$conc_tbl %>% 
      hot_to_r() %>% 
      .[1:8, 1:12] %>% 
      set_rownames(LETTERS[1:8]) -> values$conc
  })
  
  observeEvent(input$vol_tbl, {
    input$vol_tbl %>% 
      hot_to_r() %>% 
      .[1:8, 1:12] %>% 
      set_rownames(LETTERS[1:8]) -> values$vol
  })
  
  observeEvent(input$start_tecan, {
    #runWorkplan
  })
  
  
  output$conc_tbl <- renderRHandsontable({
    values$conc %>% 
      rhandsontable()
  })
  
  output$vol_tbl <- renderRHandsontable({
    values$vol %>% 
      rhandsontable()
  })
  
  output$start_btn <- renderUI({
    validate(need(work_plan(), ''))
    actionButton('start_tecan', 'Confirm and start TECAN')
  })
  
  output$work_plan_DT <- renderDataTable({
    work_plan() %>% 
      datatable(options=list(dom='ft', paging=F),
                class='compact hover nowrap order-column row-border stripe',
                rownames=F)
  })


})
