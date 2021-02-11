list(
  box(h1("SORP Calculator & Drawdown Simulation"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Parameters",
                           style = "margin-top:1em",
                           sliderInput("age_sd", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = 105),
                           radioButtons("relationship_sd", "Relationship Status:", choices = list("Single" = 1, "Married" = 2)),
                           numericInputIcon(inputId = "sal_sd", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "fundvalue_sd", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("PreK_sd", "Contribution Frequency:", freq_list),
                           selectInput("PostK_sd", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "emp_contri_sd", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "empr_contri_sd", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  #Note: any inputs here must be included in the server code for the reset button
                  tabPanel("Drawdown Parameters",
                           style = "margin-top:1em",
                           numericInputIcon(inputId = "retire_age_sd", label = "Age at Retirement:", value = 66, min = 55, max = 105, icon = list(NULL, "Years")),
                           numericInputIcon(inputId = "annual_withdrawals_sd", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
                           selectInput("withdraw_freq_sd", "Withdrawal Frequency:", freq_list),
                           numericInputIcon(inputId = "annual_mean_return_sd", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annual_ret_std_dev_sd", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annual_inflation_sd", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annual_inf_std_dev_sd", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "n_sim_sd", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                           actionButton(inputId = "resim_sd", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                  ), 
                  
                  tabPanel("SORP Assumptions",
                           style = "margin-top:1em",
                           numericInputIcon(inputId = "salEsc_sd", label = "Salary Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "discountRate_sd", label = "Discount Rate from FV to CV:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "iPost_sd", label = "Interest Rate for Annuity:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annEsc_sd", label = "Annunity Escalation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "guaranteed_sd", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
                           h4(strong("Percentage of Fund Held In:")),
                           sliderInput("equity_sd", "Equity/Property:", min = 0, max = 100, value = 40, step = 1),
                           sliderInput("fixed_sd", "Fixed Interest Securities:", min = 0, max = 60, value = 30, step = 1),
                           sliderInput("cash_sd", "Cash/Other:", min = 0, max = 100, value = 30, step = 1),
                           numericInputIcon(inputId = "investCharge_sd", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "default_sd", label = "Reset to Default", style = "background-color: white")
                  )
      ),
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Summary",
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundFV_sd")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentFV_sd"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundCV_sd")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentCV_sd"))
                           ),
                           box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot_sd"))
                  ),
                  
                  tabPanel("SORP Table",
                           style = "margin-top:1em",
                           box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_sd"),rownames= FALSE, style = "height:750px; overflow-y: scroll;overflow-x: scroll;")
                  ),
                  
                  tabPanel("Drawdown Simulations", 
                           style = "margin-top:1em",
                           box(title = "Probability of Ruin", status = "primary", width = 6, solidHeader = T,
                               h3(textOutput("drawdown_ruin_prob_sd"))),
                           box(title = "Average Fund Value", status = "primary", width = 6, solidHeader = T,
                               h3(textOutput("drawdown_average_fund_sd"))),
                           box(title = "Life Expectancy", status = 'primary', width = 6, solidHeader = T,
                               h3(textOutput('life_ex_sd'))),
                           box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot_sd")))
      )
    )
  )
)