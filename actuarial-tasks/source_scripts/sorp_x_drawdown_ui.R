list(
  box(h1("SORP Calculator & Drawdown Simulation"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("SORP Parameters",
                           style = "margin-top:1em",
                           sliderInput("sd_age", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = getOmega(ILT15_female_reduced)),
                           awesomeRadio("sd_relationship", "Relationship Status:", choices = list("Single" = 1, "Married" = 2), inline = TRUE),
                           numericInputIcon(inputId = "sd_salary", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "sd_current_fundvalue", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("sd_pre_freq", "Contribution Frequency:", freq_list),
                           selectInput("sd_post_freq", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "sd_emp_contri", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_empr_contri", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "sd_resim1", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                  ),
                  
                  tabPanel(div(id = 'sd_sorp_parameters_spouse', "Spouse SORP Parameters"),
                           style = "margin-top:1em",
                           sliderInput("sd_age_spouse", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = getOmega(ILT15_female_reduced)),
                           numericInputIcon(inputId = "sd_salary_spouse", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "sd_current_fundvalue_spouse", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("sd_pre_freq_spouse", "Contribution Frequency:", freq_list),
                           selectInput("sd_post_freq_spouse", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "sd_emp_contri_spouse", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_empr_contri_spouse", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "sd_resim2", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                  ),
                  
                  tabPanel("Drawdown Parameters",
                           style = "margin-top:1em",
                           selectInput("sd_withdraw_freq", "Withdrawal Frequency:", freq_list),
                           awesomeRadio("sd_withdraw_type", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           numericInputIcon(inputId = "sd_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "sd_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "sd_resim3", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                           
                           
                  ), 
                  
                  tabPanel("SORP Assumptions",
                           style = "margin-top:1em",
                           numericInputIcon(inputId = "sd_salary_esc", label = "Salary Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_discount_rate", label = "Discount Rate:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_annuity_interest", label = "Interest Rate for Annuity:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_annuity_esc", label = "Annuity Escalation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "sd_guaranteed", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
                           numericInputIcon(inputId = "sd_investment_charges", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           h4(strong("Equity/Property:")),
                           numericInputIcon(inputId = "sd_equity_rate", label = "Rate:", value = 4.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("sd_equity_prop", label = "Proportion:", min = 0, max = 100, value = 40, step = 1),
                           h4(strong("Fixed Interest Securities:")),
                           numericInputIcon(inputId = "sd_fixed_rate", label = "Rate:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("sd_fixed_prop", label = "Proportion:", min = 0, max = 60, value = 30, step = 1),
                           h4(strong("Cash/Other:")),
                           numericInputIcon(inputId = "sd_cash_rate", label = "Rate:", value = 0, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           sliderInput("sd_cash_prop", label = "Proportion:", min = 0, max = 100, value = 30, step = 1),
                           actionButton(inputId = "sd_resim4", label = "Re-Run Simulation", style = "background-color: white", icon("random")),
                           actionButton(inputId = "sd_default", label = "Reset to Default", style = "background-color: white")
                  )
      ),
    ),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("SORP Summary",
                             style = "margin-top:1em",
                             box(title = "Future Values", status = "primary", solidHeader = T,
                                 h4("Fund Value At Retirement:"),
                                 h3(textOutput("sd_text_fundvalue")),
                                 hr(),
                                 h4("Periodic Pension Payment:"),
                                 h3(textOutput("sd_text_pension_payment"))
                             ),
                             box(title = "Current Values", status = "primary", solidHeader = T,
                                 h4("Fund Value At Retirement:"),
                                 h3(textOutput("sd_text_fundvalue_discounted")),
                                 hr(),
                                 h4("Periodic Pension Payment:"),
                                 h3(textOutput("sd_text_pension_payment_discounted"))
                             ),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Accumulated Wealth",
                                                  style = "margin-top:1em",
                                                  box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("sd_plot_fundvalue")),                                       
                                         ),
                                         tabPanel("SORP Table",
                                                  style = "margin-top:1em",
                                                  box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("sd_table_contributions"), rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                         )
                             )
                    ),
                    
                    tabPanel(div(id = 'sd_spouse_sorp_summary', "Spouse SORP Summary"),
                             style = "margin-top:1em",
                             box(title = "Future Values", status = "primary", solidHeader = T,
                                 h4("Fund Value At Retirement:"),
                                 h3(textOutput("sd_text_fundvalue_spouse")),
                                 hr(),
                                 h4("Periodic Pension Payment:"),
                                 h3(textOutput("sd_text_pension_payment_spouse"))
                             ),
                             box(title = "Current Values", status = "primary", solidHeader = T,
                                 h4("Fund Value At Retirement:"),
                                 h3(textOutput("sd_text_fundvalue_discounted_spouse")),
                                 hr(),
                                 h4("Periodic Pension Payment:"),
                                 h3(textOutput("sd_text_pension_payment_discounted_spouse"))
                             ),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Accumulated Wealth",
                                                  style = "margin-top:1em",
                                                  box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("sd_plot_fundvalue_spouse")),                                       
                                         ),
                                         tabPanel("SORP Table",
                                                  style = "margin-top:1em",
                                                  box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("sd_table_contributions_spouse"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                         )
                             )
                    ),
                    
                    tabPanel("Drawdown Simulations",
                             style = "margin-top:1em",
                             box(
                               title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
                               h3(textOutput('sd_text_life_ex'))
                             ),
                             box(
                               title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
                               h3(textOutput("sd_text_average_fund_life_ex"))
                             ),
                             box(
                               title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
                               h3(textOutput("sd_text_ruin_prob_life_ex"))
                             ),
                             box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("sd_table"), rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
                             box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("sd_plot_sims"))
                             )
        )
      )
    )
)