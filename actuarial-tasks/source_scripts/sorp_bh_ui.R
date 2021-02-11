list(
  box(h1("SORP Calculator & Drawdown Simulation"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Parameters",
                           style = "margin-top:1em",
                           sliderInput("age_bh", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = 105),
                           radioButtons("relationship_bh", "Relationship Status:", choices = list("Single" = 1, "Married" = 2)),
                           numericInputIcon(inputId = "widowed_bh", label = "Age Widowed:", value = NA, min = 16, max = 105, icon = list(NULL, " Years Old")),
                           numericInputIcon(inputId = "sal_bh", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "fundvalue_bh", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("PreK_bh", "Contribution Frequency:", freq_list),
                           selectInput("PostK_bh", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "emp_contri_bh", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "empr_contri_bh", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  #Note: any inputs here must be included in the server code for the reset button
                  tabPanel("Drawdown Parameters",
                           style = "margin-top:1em",
                           numericInputIcon(inputId = "retire_age_bh", label = "Age at Retirement:", value = 66, min = 55, max = 105, icon = list(NULL, "Years")),
                           numericInputIcon(inputId = "annual_withdrawals_bh", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
                           selectInput("withdraw_freq_bh", "Withdrawal Frequency:", freq_list),
                           numericInputIcon(inputId = "annual_mean_return_bh", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annual_ret_std_dev_bh", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annual_inflation_bh", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annual_inf_std_dev_bh", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "n_sim_bh", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                           actionButton(inputId = "resim_bh", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                  ), 
                  
                  tabPanel("SORP Assumptions",
                           style = "margin-top:1em",
                           numericInputIcon(inputId = "salEsc_bh", label = "Salary Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "discountRate_bh", label = "Discount Rate from FV to CV:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "iPost_bh", label = "Interest Rate for Annuity:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annEsc_bh", label = "Annunity Escalation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "guaranteed_bh", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
                           h4(strong("Percentage of Fund Held In:")),
                           sliderInput("equity_bh", "Equity/Property:", min = 0, max = 100, value = 40, step = 1),
                           sliderInput("fixed_bh", "Fixed Interest Securities:", min = 0, max = 60, value = 30, step = 1),
                           sliderInput("cash_bh", "Cash/Other:", min = 0, max = 100, value = 30, step = 1),
                           numericInputIcon(inputId = "investCharge_bh", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "default_bh", label = "Reset to Default", style = "background-color: white")
                  )
      ),
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Summary",
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundFV_bh")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentFV_bh"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundCV_bh")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentCV_bh"))
                           ),
                           box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot_bh"))
                  ),
                  
                  tabPanel("SORP Table",
                           style = "margin-top:1em",
                           box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_bh"),rownames= FALSE, style = "height:750px; overflow-y: scroll;overflow-x: scroll;")
                  ),
                  
                  tabPanel("Drawdown Simulations", 
                           style = "margin-top:1em",
                           box(title = "Probability of Ruin", status = "primary", width = 6, solidHeader = T,
                               h3(textOutput("drawdown_ruin_prob_bh"))),
                           box(title = "Average Fund Value", status = "primary", width = 6, solidHeader = T,
                               h3(textOutput("drawdown_average_fund_bh"))),
                           box(title = "Life Expectancy", status = 'primary', width = 6, solidHeader = T,
                               h3(textOutput('life_ex_bh'))),
                           box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot_bh")))
      )
    )
  )
)