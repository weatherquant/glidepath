list(
# Reactive Functions ------------------------------------------------------
    import_calcs_reactive <- reactive({
    validate(
      need(input$import_database != '', "Please Upload a Database")
    )
    database <- input$import_database
    cnames <- read_excel(database$datapath, sheet = 1, n_max = 0) %>% names()
    database <- read_xlsx(database$datapath, sheet = 1, skip = 1, col_names = cnames)
    database <- data.frame(database)
    for(i in 1:length(database[, 1])){
      import_annuity = SORP_Annuity(age_1 = database[i, 1], 
                                  age_2 = database[i, 2], 
                                  relationship = database[i, 3], 
                                  freq = database[i, 7], 
                                  annuity_interest = input$import_annuity_interest, 
                                  annuity_esc = input$import_annuity_esc, 
                                  guaranteed = input$import_guaranteed)
      
      import_contributions = SORP_Contributions(age_1 = database[i, 1], 
                                              age_2 = database[i, 2], 
                                              salary = database[i, 4], 
                                              current_fundvalue = database[i, 5], 
                                              freq = database[i, 6], 
                                              emp_contri = database[i, 8], 
                                              empr_contri = database[i, 9], 
                                              salary_esc = input$import_salary_esc, 
                                              investment_charges = input$import_investment_charges, 
                                              equity_prop = input$import_equity_prop,
                                              equity_rate = input$import_equity_rate,
                                              fixed_prop = input$import_fixed_prop, 
                                              fixed_rate = input$import_fixed_rate, 
                                              cash_prop = input$import_cash_prop, 
                                              cash_rate = input$import_cash_rate)
      
      import_fund = SORP_Fund(SORP_Contributions = import_contributions)
      import_pension_payment = SORP_Pension_Payment(SORP_Fund = import_fund, SORP_Annuity = import_annuity, freq = database[i, 7])
      
      database[i, 10] = import_fund
      database[i, 11] = import_pension_payment
      database[i, 12] = SORP_Discount(x = import_fund, age_1 = database[i, 1], age_2 = database[i, 2], discount_rate = input$import_discount_rate)
      database[i, 13] = SORP_Discount(x = import_pension_payment, age_1 = database[i, 1], age_2 = database[i, 2], discount_rate = input$import_discount_rate)
    }
    colnames(database)[10:13] = c("Future Fund Value", "Future Periodic Payment", "Discounted Fund Value", "Discounted Periodic Payment")
    return(database)
  }),

# Output Functions --------------------------------------------------------
  output$import_calcs_table <- renderDataTable({
    database = import_calcs_reactive()
    database <- datatable(database[, 10:length(database[1, ])], options = list(scrollX = TRUE, scrollY = "430px", paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = TRUE)
    database <- formatCurrency(database, columns = 1:4, currency = "€")
    return(database)
  }),

# Download Functions ------------------------------------------------------
  output$import_template_download <- downloadHandler(
    filename = function() {
      paste0("sorp_database_template", ".xlsx")
    },
    content = function(file) {
      file.copy("www/sorp_database_template.xlsx", file)
  }),
  
  output$import_example_download <- downloadHandler(
    filename = function() {
      paste0("sorp_database_example", ".xlsx")
    },
    content = function(file) {
      file.copy("www/sorp_database_example.xlsx", file)
    }),
  
  output$import_calcs_download <- downloadHandler(
    filename = function() {
      paste0("sorp_database_export", ".xlsx")
    },
    content = function(file) {
      database = import_calcs_reactive()
      write_xlsx(database, file)
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$import_default, {
    updateNumericInputIcon(session, "import_salary_esc", value = 1.5)
    updateNumericInputIcon(session, "import_discount_rate", value = 2.5)
    updateNumericInputIcon(session, "import_annuity_interest", value = 0.5)
    updateNumericInputIcon(session, "import_annuity_esc", value = 1)
    updateNumericInputIcon(session, "import_guaranteed", value = 5)
    updateNumericInputIcon(session, "import_investment_charges", value = 0.5)
    updateSliderInput(session, "import_equity_prop", value = 40)
    updateSliderInput(session, "import_equity_rate", value = 4.5)
    updateSliderInput(session, "import_fixed_prop", value = 30)
    updateSliderInput(session, "import_fixed_rate", value = 1)
    updateSliderInput(session, "import_cash_prop", value = 30)
    updateSliderInput(session, "import_cash_rate", value = 0)
  }),
  
  observeEvent(input$import_equity_prop,{
    updateSliderInput(session, "import_fixed_prop", max = 100 - input$import_equity_prop)
    disable("import_cash_prop") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),

  observe({
    updateSliderInput(session, "import_cash_prop", value = 100 - input$import_equity_prop - input$import_fixed_prop)
  }),

  observeEvent(import_calcs_reactive(),{
    shinyjs::show(id = "import_calcs_download_button")
  })
)