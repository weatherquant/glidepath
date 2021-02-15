list(
  sorp_multiple <- reactive({
    sorp_database <- input$sorp_database
    cnames <- read_excel(sorp_database$datapath, sheet = 1, n_max = 0) %>%
      names()
    sorp_database <- read_xlsx(sorp_database$datapath, sheet = 1, skip=1, col_names = cnames)
    sorp_database <- data.frame(sorp_database)
    for(i in 1:length(sorp_database[, 1])){
      SORP = SORP(sorp_database[i, 1], sorp_database[i, 2], sorp_database[i, 3], sorp_database[i, 4], sorp_database[i, 5], sorp_database[i, 6], sorp_database[i, 7], sorp_database[i, 8], sorp_database[i, 9], input$salEsc_import, input$iPost_import, input$annEsc_import, input$guaranteed_import, input$equity_import, input$fixed_import, input$cash_import, input$investCharge_import, input$equity_p_import, input$fixed_p_import, input$cash_p_import)
      sorp_database[i, 10] = SORP[length(SORP[, 7]), 7]
      sorp_database[i, 11] = SORP[1, 8]
    }
    colnames(sorp_database)[10:11] = c("Future Fund Value", "Annunity Value")
    return(sorp_database)
  }),
  
  output$sorp_export_table <- renderDataTable({
    sorp = sorp_multiple()
    sorp <- datatable(sorp, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    return(sorp)
  })
)