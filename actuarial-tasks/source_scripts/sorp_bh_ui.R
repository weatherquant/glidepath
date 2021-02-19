list(
  box(h1("SORP Calculator & Drawdown Simulation"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Parameters",
                           style = "margin-top:1em",
                           sliderInput("age_bh", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = 105),
                           radioButtons("relationship_bh", "Relationship Status:", choices = list("Single" = 1, "Married" = 2, "Widowed" = 3)),
                           numericInputIcon(inputId = "widowed_bh", label = "Age Widowed:", value = 70, min = 16, max = 105, icon = list(NULL, " Years Old")),
                           numericInputIcon(inputId = "sal_bh", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "fundvalue_bh", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("PreK_bh", "Contribution Frequency:", freq_list),
                           selectInput("PostK_bh", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "emp_contri_bh", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "empr_contri_bh", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  tabPanel(div(id = 'spouse_sorp_parameters', "Spouse SORP Parameters"),
                           style = "margin-top:1em",
                           sliderInput("age_bh_s", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = 105),
                           numericInputIcon(inputId = "sal_bh_s", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "fundvalue_bh_s", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("PreK_bh_s", "Contribution Frequency:", freq_list),
                           selectInput("PostK_bh_s", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "emp_contri_bh_s", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "empr_contri_bh_s", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  #Note: any inputs here must be included in the server code for the reset button
                  tabPanel("Drawdown Parameters",
                           style = "margin-top:1em",
                           radioButtons("percent_yn_bh", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T)),
                           numericInputIcon(inputId = "annual_withdrawals_bh", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "percent_withdrawal_bh", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
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
                           numericInputIcon(inputId = "investCharge_bh", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           h4(strong("Equity/Property:")),
                           numericInputIcon(inputId = "equity_p_bh", label = "Rate:", value = 4.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("equity_bh", label = "Proportion:", min = 0, max = 100, value = 40, step = 1),
                           h4(strong("Fixed Interest Securities:")),
                           numericInputIcon(inputId = "fixed_p_bh", label = "Rate:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("fixed_bh", label = "Proportion:", min = 0, max = 60, value = 30, step = 1),
                           h4(strong("Cash/Other:")),
                           numericInputIcon(inputId = "cash_p_bh", label = "Rate:", value = 0, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("cash_bh", label = "Proportion:", min = 0, max = 100, value = 30, step = 1),
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
                           tabsetPanel(type = "tabs",
                                       tabPanel("Accumulated Wealth",
                                                style = "margin-top:1em",
                                                box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot_bh")),                                       
                                                ),
                                       tabPanel("SORP Table",
                                                style = "margin-top:1em",
                                                box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_bh"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                                )
                                       )
                  ),
                  
                  tabPanel(div(id = 'spouse_sorp_summary', "Spouse SORP Summary"),
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundFV_bh_s")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentFV_bh_s"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundCV_bh_s")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentCV_bh_s"))
                           ),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Accumulated Wealth",
                                                style = "margin-top:1em",
                                                box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot_bh_s")),                                       
                                       ),
                                       tabPanel("SORP Table",
                                                style = "margin-top:1em",
                                                box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_bh_s"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       )
                           )
                  ),
                  
                  tabPanel("Drawdown Simulations",
                           style = "margin-top:1em",
                           div(id = "life_ex_box_bh",box(
                             title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
                             h3(textOutput('life_ex_bh')))
                           ),
                           div(id = "average_fund_box_bh",box(
                               title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
                               h3(textOutput("drawdown_average_fund_bh")))
                               ),
                           div(id = "prob_ruin_box_bh", box(
                             title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
                             h3(textOutput("drawdown_ruin_prob_bh")))
                           ),
                           box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_d_bh"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
                           box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot_bh")))
      )
    )
  )
)