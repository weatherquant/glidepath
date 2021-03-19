list(
  box(h1("SORP Import Tool"), width = 12, background = "light-blue"),
  box(title = "SORP File Upload", status = "primary", solidHeader = T, width = 12,
      fileInput("import_database", "Choose Database File", accept = c(".xls", ".xlsx", ".xlsm")),
      textOutput("test"),
      downloadButton("import_template_download", label = "Download Template"),
      downloadButton("import_example_download", label = "Download Example")
  ),

  box(
    title = "SORP Output", status = "primary", solidHeader = T, width = 12,
    DT::dataTableOutput("import_calcs_table"),
    br(),
    hidden(div(id = "import_calcs_download_button", downloadButton("import_calcs_download", label = "Download Data"), style = "background-color: white; float:right"))
  ),
  
  box(title = "SORP Assumptions", status = "primary", solidHeader = T, width = 12, collapsible = T, collapsed = T,
      h4(strong("General Assumptions:")),
      numericInputIcon(inputId = "import_salary_esc", label = "Salary Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "import_discount_rate", label = "Discount Rate:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "import_annuity_interest", label = "Interest Rate for Annuity:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "import_annuity_esc", label = "Annuity Escalation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "import_guaranteed", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
      numericInputIcon(inputId = "import_investment_charges", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      hr(),
      h4(strong("Equity/Property:")),
      numericInputIcon(inputId = "import_equity_rate", label = "Rate:", value = 4.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      sliderInput("import_equity_prop", label = "Proportion:", min = 0, max = 100, value = 40, step = 1),
      h4(strong("Fixed Interest Securities:")),
      numericInputIcon(inputId = "import_fixed_rate", label = "Rate:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      sliderInput("import_fixed_prop", label = "Proportion:", min = 0, max = 60, value = 30, step = 1),
      h4(strong("Cash/Other:")),
      numericInputIcon(inputId = "import_cash_rate", label = "Rate:", value = 0, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      sliderInput("import_cash_prop", label = "Proportion:", min = 0, max = 100, value = 30, step = 1),
      actionButton(inputId = "import_default", label = "Reset to Default", style = "background-color: white")
  )
)