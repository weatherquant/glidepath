list(
  box(h1("SORP Import Tool"), width = 12, background = "light-blue"),
  box(title = "SORP File Upload", status = "primary", solidHeader = T, width = 12,
      fileInput("sorp_database", "Choose Database File", accept = c(".xls", ".xlsx", ".xlsm")),
      textOutput("test"),
      downloadButton("download_template_import", label = "Download Template")
  ),
  
  box(title = "SORP Assumptions", status = "primary", solidHeader = T, width = 12, collapsible = T, collapsed = T,
           numericInputIcon(inputId = "salEsc_import", label = "Salary Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           numericInputIcon(inputId = "discountRate_import", label = "Discount Rate from FV to CV:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           numericInputIcon(inputId = "iPost_import", label = "Interest Rate for Annuity:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           numericInputIcon(inputId = "annEsc_import", label = "Annunity Escalation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           numericInputIcon(inputId = "guaranteed_import", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
           numericInputIcon(inputId = "investCharge_import", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           h4(strong("Equity/Property:")),
           numericInputIcon(inputId = "equity_p_import", label = "Rate:", value = 4.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           sliderInput("equity_import", label = "Proportion:", min = 0, max = 100, value = 40, step = 1),
           h4(strong("Fixed Interest Securities:")),
           numericInputIcon(inputId = "fixed_p_import", label = "Rate:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           sliderInput("fixed_import", label = "Proportion:", min = 0, max = 60, value = 30, step = 1),
           h4(strong("Cash/Other:")),
           numericInputIcon(inputId = "cash_p_import", label = "Rate:", value = 0, min = 0, max = 100, icon = list(NULL, icon("percent"))),
           sliderInput("cash_import", label = "Proportion:", min = 0, max = 100, value = 30, step = 1),
           actionButton(inputId = "default_import", label = "Reset to Default", style = "background-color: white")
  ),

  box(
    title = p("SORP Output", hidden(div(id = "export_button", downloadButton("download_template_export", label = "Download Data")))), status = "primary", solidHeader = T, width = 12,
    DT::dataTableOutput("sorp_export_table"), style = "height:430px; overflow-y: scroll;overflow-x: scroll;"
  )
)