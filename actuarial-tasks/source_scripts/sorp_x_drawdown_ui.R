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
                  
                  tabPanel(div(id = 'spouse_sorp_parameters', "Spouse SORP Parameters"),
                           style = "margin-top:1em",
                           sliderInput("age_sd_s", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = 105),
                           numericInputIcon(inputId = "sal_sd_s", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "fundvalue_sd_s", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("PreK_sd_s", "Contribution Frequency:", freq_list),
                           selectInput("PostK_sd_s", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "emp_contri_sd_s", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "empr_contri_sd_s", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  #Note: any inputs here must be included in the server code for the reset button
                  tabPanel("Drawdown Parameters",
                           style = "margin-top:1em",
                           radioButtons("percent_yn_sd", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T)),
                           numericInputIcon(inputId = "annual_withdrawals_sd", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "percent_withdrawal_sd", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
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
                           numericInputIcon(inputId = "investCharge_sd", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           h4(strong("Equity/Property:")),
                           numericInputIcon(inputId = "equity_p_sd", label = "Rate:", value = 4.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("equity_sd", label = "Proportion:", min = 0, max = 100, value = 40, step = 1),
                           h4(strong("Fixed Interest Securities:")),
                           numericInputIcon(inputId = "fixed_p_sd", label = "Rate:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("fixed_sd", label = "Proportion:", min = 0, max = 60, value = 30, step = 1),
                           h4(strong("Cash/Other:")),
                           numericInputIcon(inputId = "cash_p_sd", label = "Rate:", value = 0, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("cash_sd", label = "Proportion:", min = 0, max = 100, value = 30, step = 1),
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
                           tabsetPanel(type = "tabs",
                                       tabPanel("Accumulated Wealth",
                                                style = "margin-top:1em",
                                                box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot_sd")),                                       
                                       ),
                                       tabPanel("SORP Table",
                                                style = "margin-top:1em",
                                                box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_sd"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       )
                           )
                  ),
                  
                  tabPanel(div(id = 'spouse_sorp_summary', "Spouse SORP Summary"),
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundFV_sd_s")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentFV_sd_s"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundCV_sd_s")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentCV_sd_s"))
                           ),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Accumulated Wealth",
                                                style = "margin-top:1em",
                                                box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot_sd_s")),                                       
                                       ),
                                       tabPanel("SORP Table",
                                                style = "margin-top:1em",
                                                box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_sd_s"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       )
                           )
                  ),
                  
                  tabPanel("Drawdown Simulations",
                           style = "margin-top:1em",
                           div(id = "life_ex_box_sd",box(
                             title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
                             h3(textOutput('life_ex_sd')))
                           ),
                           div(id = "average_fund_box_sd",box(
                             title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
                             h3(textOutput("drawdown_average_fund_sd")))
                           ),
                           div(id = "prob_ruin_box_sd", box(
                             title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
                             h3(textOutput("drawdown_ruin_prob_sd")))
                           ),
                           box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_d_sd"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
                           box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot_sd")))
      )
    )
  )
)