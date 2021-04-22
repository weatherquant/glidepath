list(
  box(h1("SORP Calculator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(style = "height:737px; overflow-y:auto",
      
      tabsetPanel(type = "tabs",
                  tabPanel("Parameters",
                           style = "margin-top:1em",
                           sliderInput("sorp_age", "Current Age and Retirement Age:", value = c(45, 66), min = 16, max = getOmega(ILT15_female_reduced)),
                           awesomeRadio("sorp_relationship", "Relationship Status:", choices = list("Single" = 1, "Married" = 2), inline = TRUE),
                           numericInputIcon(inputId = "sorp_salary", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "sorp_current_fundvalue", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("sorp_pre_freq", "Contribution Frequency:", freq_list),
                           selectInput("sorp_post_freq", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "sorp_emp_contri", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sorp_empr_contri", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  tabPanel("Assumptions",
                           style = "margin-top:1em",
                           h4(strong("General Assumptions:")),
                           numericInputIcon(inputId = "sorp_salary_esc", label = "Salary Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sorp_discount_rate", label = "Discount Rate:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sorp_annuity_interest", label = "Interest Rate for Annuity:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sorp_annuity_esc", label = "Annuity Escalation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sorp_guaranteed", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
                           numericInputIcon(inputId = "sorp_investment_charges", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           hr(),
                           h4(strong("Equity/Property:")),
                           numericInputIcon(inputId = "sorp_equity_rate", label = "Rate:", value = 4.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("sorp_equity_prop", label = "Proportion:", min = 0, max = 100, value = 40, step = 1),
                           h4(strong("Fixed Interest Securities:")),
                           numericInputIcon(inputId = "sorp_fixed_rate", label = "Rate:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("sorp_fixed_prop", label = "Proportion:", min = 0, max = 60, value = 30, step = 1),
                           h4(strong("Cash/Other:")),
                           numericInputIcon(inputId = "sorp_cash_rate", label = "Rate:", value = 0, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("sorp_cash_prop", label = "Proportion:", min = 0, max = 100, value = 30, step = 1),
                           actionButton(inputId = "sorp_default", label = "Reset to Default", style = "background-color: white")
                  )
      ),
    ),
    
    mainPanel(
      box(title = "Future Values", status = "primary", solidHeader = T,
          h4("Fund Value At Retirement:"),
          h3(textOutput("sorp_text_fundvalue")),
          hr(),
          h4("Periodic Pension Payment:"),
          h3(textOutput("sorp_text_pension_payment"))
      ),
      box(title = "Discounted Values", status = "primary", solidHeader = T,
          h4("Fund Value At Retirement:"),
          h3(textOutput("sorp_text_fundvalue_discounted")),
          hr(),
          h4("Periodic Pension Payment:"),
          h3(textOutput("sorp_text_pension_payment_discounted"))
      ),
      tabBox(type = "tabs", width = 12,
             tabPanel("Accumulated Wealth", plotOutput("sorp_plot_fundvalue")),
             tabPanel("Contributions and Fund Value over Time", DT::dataTableOutput("sorp_table_contributions"), rownames = FALSE)
      ),
    )
  )
)
     