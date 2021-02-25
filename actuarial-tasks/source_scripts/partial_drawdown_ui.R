list(
  box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Common Parameters",
                           style = "margin-top:1em",
                           sliderInput("age_pd", "Drawdown Age and Annuity Age:", value = c(66, 76), min = 55, max = getOmega(ILT15_female_reduced)),
                           awesomeRadio("relationship_pd", "Relationship Status:", choices = list("Single" = 1, "Married" = 2), inline = TRUE),
                           numericInputIcon(inputId = "start_capital_pd", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                           # selectInput("withdraw_freq_pd", "Withdrawal Frequency:", freq_list)
                           ),
                 
                  tabPanel("Full Drawdown Withdrawals",
                           style = "margin-top:1em",
                           awesomeRadio("percent_yn_pd_d", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), selected = F, inline = TRUE),
                           numericInputIcon(inputId = "annual_withdrawals_pd_d", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "percent_withdrawal_pd_d", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))
                          ),
                  
                  tabPanel("Buy Annuity Later Withdrawals",
                           style = "margin-top:1em",
                           awesomeRadio("percent_yn_pd_bl", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), selected = F, inline = TRUE),
                           numericInputIcon(inputId = "annual_withdrawals_pd_bl", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "percent_withdrawal_pd_bl", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))
                  ),
                  
                  tabPanel("Deferred Annuity Withdrawals",
                           style = "margin-top:1em",
                           awesomeRadio("percent_yn_pd_da", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), selected = F, inline = TRUE),
                           numericInputIcon(inputId = "annual_withdrawals_pd_da", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "percent_withdrawal_pd_da", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))
                  ),
                  
                  tabPanel("Drawdown Parameters",
                          style = "margin-top:1em",
                          numericInputIcon(inputId = "annual_mean_return_pd", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "annual_ret_std_dev_pd", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "annual_inflation_pd", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "annual_inf_std_dev_pd", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          actionButton(inputId = "resim_pd", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                          )
    )),
    
    mainPanel(
      div(id = "life_ex_box_pd",box(
        title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
        h3(textOutput('life_ex_pd')))
      ),
      div(id = "sorp_payment_box_pd",box(
        title = "SORP Payment", status = 'primary', solidHeader = T, width = 4,
        h3(textOutput('sorp_payment_pd')))
      ),
      div(id = "average_fund_box_pd_bl",box(
        title = "Average Fund Value (Buy Later)", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("drawdown_average_fund_pd_bl")))
      ),
      div(id = "average_annuity_box_pd_bl", box(
        title = "Average Periodic Payment (Buy Later)", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("average_annuity_pd_bl")))
      ),
      div(id = "cost_annuity_box_pd_da", box(
        title = "Cost to Buy Annuity (Deferred)", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("cost_annuity_pd_da")))
      ),
      div(id = "average_fund_box_pd_da",box(
        title = "Average Fund Value (Deferred)", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("drawdown_average_fund_pd_da")))
      ),
      div(id = "average_annuity_box_pd_da", box(
        title = "Average Periodic Payment (Deferred)", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("average_annuity_pd_da")))
      ),
      box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("compare_table_d_pd"), style = "height:430px; overflow-y: scroll;overflow-x: scroll;")    )
  )
)