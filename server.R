
shinyServer(function(input, output, session) {
  
  autoInvalidate <- reactiveTimer(2000, session)
  
  values <- reactiveValues(conc=INIT_MATRIX, vol=INIT_MATRIX, isEvowareRunning=NULL)
  
  work_plan <- reactive({
    data_frame(position=1:96,
               conc_init=as.vector(values$conc),
               vol_init=as.vector(values$vol)) %>% 
      filter(!is.na(conc_init), !is.na(vol_init)) %>% 
      buildWorkplan(vol_dead=vol_dead, vol_min=vol_min, vol_max=vol_max)
  })
  
  work_list <- eventReactive(work_plan(), {
    buildWorklist(work_plan(), vol_max, vol_min, source_plate, dest_plate)
  })
  
  work_list1 <- eventReactive(work_plan(), {
    buildWorklist1(work_plan(), vol_max, vol_min, diluent, dest_plate, n_tips)
  })
  
  
  observe({
    autoInvalidate()
    values$isEvowareRunning <- length(getPid())>0
  })
  

  observeEvent(input$load_conc, {
    input$load_conc$datapath %>% 
      readData() -> values$conc
  })
  
  observeEvent(input$conc_tbl, {
    input$conc_tbl %>% 
      hot_to_r() %>% 
      .[1:8, 1:12] %>% 
      apply(2, as.double) %>% 
      set_rownames(LETTERS[1:8]) %>% 
      set_colnames(1:12) -> values$conc
  })
  
  observeEvent(input$vol_tbl, {
    input$vol_tbl %>% 
      hot_to_r() %>% 
      .[1:8, 1:12] %>% 
      apply(2, as.double) %>% 
      set_rownames(LETTERS[1:8]) %>% 
      set_colnames(1:12) -> values$vol
  })
  
  observeEvent(input$fill, {
    values$vol <- input$uni_vol %>% 
      as.double() %>%
      rep(times=96) %>% 
      matrix(nrow=8) %>% 
      set_rownames(LETTERS[1:8]) %>% 
      set_colnames(1:12)
  })
  
  observeEvent(input$start_tecan, {
    disable('start_tecan')
    text('start_tecan', 'TECAN Running...')
    runWorkplan(work_list(), work_list1(), script_path, EVOware_path, username, password)
    text('start_tecan', 'Confirm and start TECAN')
    enable('start_tecan')
  })
  
  
  output$conc_tbl <- renderRHandsontable({
    values$conc %>% 
      rhandsontable(maxRows=8, maxCols=12, height=220)
  })
  
  output$vol_tbl <- renderRHandsontable({
    values$vol %>% 
      rhandsontable(maxRows=8, maxCols=12, height=220)
  })
  
  output$start_btn <- renderUI({
    validate(need(work_plan(), ''),
             need(!values$isEvowareRunning, 'Please shutdown Evoware before start.'))
    actionButton('start_tecan', 'Confirm and start TECAN')
  })
  
  output$work_plan_DT <- renderDataTable({
    validate(need(work_plan(), ''))
    work_plan() %>% 
      datatable(options=list(dom='ft', paging=F),
                class='compact hover nowrap order-column row-border stripe',
                rownames=F) %>% 
      formatRound(2:7, 4)
  })


})
