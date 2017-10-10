Sys.setlocale(category = "LC_ALL", locale = "sv_SE.UTF-8")
server <- function(input, output, session) {
  
  outVar = reactive({
    mydata <-unique(patient_df[, input$qst])
    return(mydata)
  })
  
  observe({
    updateSelectInput(session, "ans",
                      label  = "Fokusgrupp: Svar",
                      choices = outVar()
    )})
  
  output$plotly_age <- renderPlotly({
    plot_age_distrub
  })
  
  output$plotly_landst <- renderPlotly({
    plot_landst_distrub
  })
  
  output$plotly_stod <- renderPlotly({
    plot_stod
  })
  
  output$plotly_kontakt <- renderPlotly({
    plot_kontakt
  })
  
  output$plotly_fortroende <- renderPlotly({
    plot_fortroende
  })
  
  output$plotly_bemotande <- renderPlotly({
    plot_bemotande
  })
  
  focus_group_cnt <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.ageGroup")]
    colnames(data) <- c("Answer", "Patient.ageGroup")
    data <- data %>% filter(Answer==input$ans)
    return(nrow(data))
  })
  
  output$FocusGrp_info <- renderValueBox({
    valueBox(
      focus_group_cnt()
      ,"antal patienter i fokusgrupp"
      ,icon = icon("heartbeat")
      ,color = "purple"
    )
  })
  
  output$TotalGrp_info <- renderValueBox({
    valueBox(
      nrow(patient_df)
      ,"totalt antal patienter"
      ,icon = icon("heartbeat")
      ,color = "purple"
    )
  })
  
  age_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.ageGroup")]
    colnames(data) <- c("Answer", "Patient.ageGroup")
    data <- data %>% filter(Answer==input$ans)
    data2 <- data %>% group_by(Patient.ageGroup) %>% dplyr::summarise(FocusGrp=n()/nrow(data))
    data3 <- agegroup_total %>% left_join(data2, by="Patient.ageGroup")
    return(data3)
  })
  
  output$plotly_vs_age <- renderPlotly({
    plot_ly(age_vs_data(), x = ~Patient.ageGroup, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
  
  landst_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.Landsting")]
    colnames(data) <- c("Answer", "Patient.Landsting")
    data <- data %>% filter(Answer==input$ans)
    data2 <- data %>% group_by(Patient.Landsting) %>% dplyr::summarise(FocusGrp=n()/nrow(data))
    data3 <- ladsting_total %>% left_join(data2, by="Patient.Landsting")
    return(data3)
  })
  
  output$plotly_vs_landst <- renderPlotly({
    plot_ly(landst_vs_data(), x = ~Patient.Landsting, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
  informerad_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.HurInformerad")]
    colnames(data) <- c("Answer", "Patient.HurInformerad")
    data <- data %>% filter(Answer==input$ans)
    data2 <- data %>% group_by(Patient.HurInformerad) %>% dplyr::summarise(FocusGrp=n()/nrow(data))
    data3 <- informerad_total %>% left_join(data2, by="Patient.HurInformerad")
    return(data3)
  })
  
  output$plotly_vs_informerad <- renderPlotly({
    plot_ly(informerad_vs_data(), x = ~Patient.HurInformerad, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
  fortroende_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.Förtroende")]
    colnames(data) <- c("Answer", "Patient.Förtroende")
    data <- data %>% filter(Answer==input$ans)
    data2 <- data %>% group_by(Patient.Förtroende) %>% dplyr::summarise(FocusGrp=n()/nrow(data))
    data3 <- fortroende_total %>% left_join(data2, by="Patient.Förtroende")
    return(data3)
  })
  
  output$plotly_vs_fortroende <- renderPlotly({
    plot_ly(fortroende_vs_data(), x = ~Patient.Förtroende, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
  kontakt_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.KontaktMedDiabetiker")]
    colnames(data) <- c("Answer", "Patient.KontaktMedDiabetiker")
    data <- data %>% filter(Answer==input$ans)
    data2 <- data %>% group_by(Patient.KontaktMedDiabetiker) %>% dplyr::summarise(FocusGrp=n()/nrow(data))
    data3 <- kontakt_total %>% left_join(data2, by="Patient.KontaktMedDiabetiker")
    return(data3)
  })
  
  output$plotly_vs_kontakt <- renderPlotly({
    plot_ly(kontakt_vs_data(), x = ~Patient.KontaktMedDiabetiker, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
  stod_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, "Patient.StödUtanför")]
    colnames(data) <- c("Answer", "Patient.StödUtanför")
    data <- data %>% filter(Answer==input$ans)
    data2 <- data %>% group_by(Patient.StödUtanför) %>% dplyr::summarise(FocusGrp=n()/nrow(data))
    data3 <- stod_total %>% left_join(data2, by="Patient.StödUtanför")
    return(data3)
  })
  
  output$plotly_vs_stod <- renderPlotly({
    plot_ly(stod_vs_data(), x = ~Patient.StödUtanför, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
  question_vs_data <- reactive({
    data  <- patient_df[, c(input$qst, input$cmp)]
    colnames(data) <- c("Q1", "Q2")
    
    data_tot <- data %>% group_by(Q2) %>% summarise(Total=n()/nrow(data))
    
    data_qst <- data %>% filter(Q1==input$ans)
    data_qst <- data_qst %>% group_by(Q2) %>% summarise(FocusGrp=n()/nrow(data_qst))

    output_data <- data_tot %>% left_join(data_qst, by="Q2")
    return(output_data)
  })
  
  output$plotly_vs_question <- renderPlotly({
    plot_ly(question_vs_data(), x = ~Q2, y = ~Total, type = 'bar', name = 'Total') %>%
      add_trace(y = ~FocusGrp, name = 'FocusGrp',  marker = list(color = 'rgb(195, 240, 255)')) %>%
      layout(yaxis = list(title = 'Andel'), barmode = 'group')
  })
  
}