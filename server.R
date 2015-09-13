
shinyServer(function(input, output, session) {
  
  autoInvalidate <- reactiveTimer(2000, session)
  
  values <- reactiveValues(conc=INIT_MATRIX, vol=INIT_MATRIX, isEvowareRunning=NULL)
  
  work_plan <- reactive({
    if(input$auto_final_conc){
      conc_final <- NULL
    } else if(!input$auto_final_conc){
      conc_final <- input$user_final_conc
    }
    data_frame(position=1:(source_nrow*source_ncol),
               conc_init=as.vector(values$conc),
               vol_init=as.vector(values$vol)) %>% 
      filter(!is.na(conc_init), !is.na(vol_init)) %>% 
      buildWorkplan(vol_dead=vol_dead, vol_min=vol_min, vol_max=vol_max, conc_final=conc_final)
  })
  
  source2dest <- eventReactive(work_plan(), {
    work_plan() %>% 
      use_series(vol_source) %>% 
      .[1:(dest_nrow*dest_ncol)] %>% 
      matrix(nrow=dest_nrow) %>% 
      set_rownames(LETTERS[1:dest_nrow]) %>% 
      set_colnames(1:dest_ncol)
  })
  
  diluent2dest <- eventReactive(work_plan(), {
    work_plan() %>% 
      use_series(vol_diluent) %>% 
      .[1:(dest_nrow*dest_ncol)] %>% 
      matrix(nrow=dest_nrow) %>% 
      set_rownames(LETTERS[1:dest_nrow]) %>% 
      set_colnames(1:dest_ncol)
  })
  
  finalconc <- eventReactive(work_plan(), {
    work_plan() %>% 
      use_series(expected_final_conc) %>% 
      .[1:(dest_nrow*dest_ncol)] %>% 
      matrix(nrow=dest_nrow) %>% 
      set_rownames(LETTERS[1:dest_nrow]) %>% 
      set_colnames(1:dest_ncol)
  })
  
  finalvol <- eventReactive(work_plan(), {
    work_plan() %>% 
      use_series(expected_final_vol) %>% 
      .[1:(dest_nrow*dest_ncol)] %>% 
      matrix(nrow=dest_nrow) %>% 
      set_rownames(LETTERS[1:dest_nrow]) %>% 
      set_colnames(1:dest_ncol)
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
      .[1:source_nrow, 1:source_ncol] %>% 
      apply(2, as.double) %>% 
      set_rownames(LETTERS[1:source_nrow]) %>% 
      set_colnames(1:source_ncol) -> values$conc
  })
  
  observeEvent(input$vol_tbl, {
    input$vol_tbl %>% 
      hot_to_r() %>% 
      .[1:source_nrow, 1:source_ncol] %>% 
      apply(2, as.double) %>% 
      set_rownames(LETTERS[1:source_nrow]) %>% 
      set_colnames(1:source_ncol) -> values$vol
  })
  
  observeEvent(input$fill, {
    values$vol <- input$uni_vol %>% 
      as.double() %>%
      rep(times=source_nrow*source_ncol) %>% 
      matrix(nrow=source_nrow) %>% 
      set_rownames(LETTERS[1:source_nrow]) %>% 
      set_colnames(1:source_ncol)
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
      rhandsontable(maxRows=source_nrow, maxCols=source_ncol, height=420, width=500)
  })
  
  output$vol_tbl <- renderRHandsontable({
    values$vol %>% 
      rhandsontable(maxRows=source_nrow, maxCols=source_ncol, height=420, width=500)
  })
  
  output$source2dest_title <- renderUI({
    validate(need(source2dest(), ''))
    h3('Volumn: source to dest')
  })
  
  output$source2dest_tbl <- renderRHandsontable({
    source2dest() %>% 
      rhandsontable(readOnly=T, height=220) %>% 
      hot_table(contextMenu=F, maxRows=dest_nrow, maxCols=dest_ncol, fillHandle=F)
  })
  
  output$diluent2dest_title <- renderUI({
    validate(need(diluent2dest(), ''))
    h3('Volumn: diluent to dest')
  })
  
  output$diluent2dest_tbl <- renderRHandsontable({
    diluent2dest() %>% 
      rhandsontable(readOnly=T, height=220) %>% 
      hot_table(contextMenu=F, maxRows=dest_nrow, maxCols=dest_ncol, fillHandle=F)
  })
  
  output$finalconc_title <- renderUI({
    validate(need(finalconc(), ''))
    h3('Final concentration')
  })
  
  output$finalconc_tbl <- renderRHandsontable({
    finalconc() %>% 
      rhandsontable(readOnly=T, height=220) %>% 
      hot_table(contextMenu=F, maxRows=dest_nrow, maxCols=dest_ncol, fillHandle=F)
  })
  
  output$finalvol_title <- renderUI({
    validate(need(finalvol(), ''))
    h3('Final volumn')
  })
  
  output$finalvol_tbl <- renderRHandsontable({
    finalvol() %>% 
      rhandsontable(readOnly=T, height=220) %>% 
      hot_table(contextMenu=F, maxRows=dest_nrow, maxCols=dest_ncol, fillHandle=F)
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
