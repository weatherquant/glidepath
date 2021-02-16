list(
  sorp_multiple <- reactive({
    sorp_database <- input$sorp_database
    cnames <- read_excel(sorp_database$datapath, sheet = 1, n_max = 0) %>%
      names()
    sorp_database <- read_xlsx(sorp_database$datapath, sheet = 1, skip=1, col_names = cnames)
    sorp_database <- data.frame(sorp_database)
    for(i in 1:length(sorp_database[, 1])){
      SORP = SORP(sorp_database[i, 1], sorp_database[i, 2], sorp_database[i, 3], sorp_database[i, 4], sorp_database[i, 5], sorp_database[i, 6], sorp_database[i, 7], sorp_database[i, 8], sorp_database[i, 9], input$salEsc_import, input$iPost_import, input$annEsc_import, input$guaranteed_import, input$equity_import, input$fixed_import, input$cash_import, input$investCharge_import, input$equity_p_import, input$fixed_p_import, input$cash_p_import)
      annuity = SORP[1, 8]
      postK = p_list[match(sorp_database[i, 7], freq_list)]
      discount_factor = 1/((1 + input$discountRate_import/100)^(sorp_database[i, 2] - sorp_database[i, 1]))
      sorp_database[i, 10] = SORP[length(SORP[, 7]), 7]
      sorp_database[i, 11] = (sorp_database[i, 10]/annuity)/postK
      sorp_database[i, 12] = sorp_database[i, 10] * discount_factor
      sorp_database[i, 13] = sorp_database[i, 11] * discount_factor
    }
    colnames(sorp_database)[10:13] = c("Future Fund Value", "Future Periodic Payment", "Discounted Fund Value", "Discounted Periodic Payment")
    return(sorp_database)
  }),
  
  observeEvent(input$default_import, {
    updateNumericInputIcon(session, "salEsc_import", value = 1.5)
    updateNumericInputIcon(session, "discountRate_import", value = 2.5)
    updateNumericInputIcon(session, "iPost_import", value = 0.5)
    updateNumericInputIcon(session, "annEsc_import", value = 1)
    updateNumericInputIcon(session, "guaranteed_import", value = 5)
    updateSliderInput(session, "equity_import", value = 40)
    updateSliderInput(session, "fixed_import", value = 30)
    updateSliderInput(session, "cash_import", value = 30)
    updateNumericInputIcon(session, "investCharge_import", value = 0.5)
    updateSliderInput(session, "equity_p_import", value = 4.5)
    updateSliderInput(session, "fixed_p_import", value = 1)
    updateSliderInput(session, "cash_p_import", value = 0)
  }),
  
  output$sorp_export_table <- renderDataTable({
    sorp = sorp_multiple()
    sorp <- datatable(sorp[, 10:length(sorp[1, ])], options = list(paging = FALSE, searching = FALSE), rownames= TRUE)
    sorp <- formatCurrency(sorp, columns = 1:4, currency = "€")
    return(sorp)
  }),
  
  output$download_template_import <- downloadHandler(
    filename = function() {
      paste0("sorp_database_template", ".xlsx")
    },
    content = function(file) {
      file.copy("www/sorp_database_template.xlsx", file)
  }),
  
  observeEvent(input$sorp_database,{
    shinyjs::show(id = "export_button")
  }),
  
  output$download_template_export <- downloadHandler(
    filename = function() {
      paste0("sorp_database_export", ".xlsx")
    },
    content = function(file) {
      sorp = sorp_multiple()
      write_xlsx(sorp, file)
    })
)