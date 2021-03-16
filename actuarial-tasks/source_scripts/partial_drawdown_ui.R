list(
  box(h1("Retirement Products"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Common Parameters",
                           style = "margin-top:1em",
                           sliderInput("pd_age", "Drawdown Age and Annuity Age:", value = c(66, 76), min = 55, max = getOmega(ILT15_female_reduced), step = 1),
                           awesomeRadio("pd_relationship", "Relationship Status:", choices = list("Single" = 1, "Married" = 2), inline = TRUE),
                           numericInputIcon(inputId = "pd_starting_capital", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                           selectInput("pd_withdraw_freq", "Withdrawal Frequency:", freq_list_drawdown),
                           actionButton(inputId = "pd_resim1", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                           ),

                  tabPanel("Withdrawal Parameters",
                           style = "margin-top:1em",
                           h4(strong("100% Drawdown:")),
                           awesomeRadio("pd_withdraw_type_drawdown", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           numericInputIcon(inputId = "pd_annual_withdrawals_drawdown", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "pd_percent_withdrawal_drawdown", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           hr(),
                           h4(strong("Drawdown and Subsequent Annuity Purchase:")),
                           awesomeRadio("pd_withdraw_type_buylater", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           numericInputIcon(inputId = "pd_annual_withdrawals_buylater", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "pd_percent_withdrawal_buylater", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           hr(),
                           h4(strong("Deferred Annuity and Drawdown:")),
                           style = "margin-top:1em",
                           awesomeRadio("pd_withdraw_type_deferred", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           numericInputIcon(inputId = "pd_annual_withdrawals_deferred", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "pd_percent_withdrawal_deferred", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "pd_resim2", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                           ),

                  tabPanel("Market Parameters",
                          style = "margin-top:1em",
                          h5(strong("Portfolio Suggestion Tool:")),
                          actionButton(inputId = "pd_surveydisplay", label = "Risk Profiler", style = "background-color: white", icon("poll")),
                          h6(textOutput("pd_save_results_text")),
                          numericInputIcon(inputId = "pd_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "pd_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "pd_annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "pd_annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          actionButton(inputId = "pd_resim3", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                          )
      ),
    ),
    
    
    mainPanel(
      
      box(status = "primary", width = 12, solidHeader = F, 
          h3("The Four Retirement Products"),
          h5("1. Purchase an annuity with the inital retirement fund."),
          h5("2. Drawdown the retirement fund for the remainder of lifetime."),
          h5("3. The initial fund is drawdown for n years. The fund value at the end of that 
             that period is used to purchase an annuity."),
          h5("4. Upon retirement a deferred annuity, with the same periodic payment as the annuity in the 
             first scenario, is purchased. The remainder of the inital reitrement fund is drawdown for the deferred period.")
      ),
      
      box(title = "100% Annuity", status = "primary", solidHeader = T,
          h4("Periodic Annuity Payment:"),
          h3(textOutput("pd_text_annuity_payment")),
          hr(),
          h4("Total Annuity Payments Received:"),
          h3(textOutput("pd_text_annuity_cumulative_life_ex"))
      ),
      
      box(title = "100% Drawdown", status = "primary", solidHeader = T,
          h4("Total Payments Received:"),
          h3(textOutput("pd_text_drawdown_total_withdrawals_life_ex")),
          hr(),
          h4("Average Final Fund Value:"),
          h3(textOutput("pd_text_drawdown_average_fund_life_ex"))
      ),
      
      box(title = "Drawdown followed by Annuity Purchase", status = "primary", solidHeader = T,
          h4("Fund Value At End of Drawdown Period:"),
          h3(textOutput("pd_text_buylater_average_fund_end")),
          hr(),
          h4("Periodic Annuity Payment:"),
          h3(textOutput("pd_text_average_annuity_payment_buylater"))
      ),
      
      box(title = "Drawdown and Deferred Annuity", status = "primary", solidHeader = T,
          h4("Cost of Deferred Annuity:"),
          h3(textOutput("pd_text_annuity_cost_deferred")),
          hr(),
          h4("Periodic Annuity Payment:"),
          h3(textOutput("pd_text_annuity_payment_deferred"))
      ),
      
      tabBox(
        title = "Partial Drawdown Visuals",
        id = "tabset1", width = 12,height = "530px",
        tabPanel("Retirement Income", plotlyOutput("pd_plot_income_compare")),
        tabPanel("Table",  DT::dataTableOutput("pd_comparison_table"), rownames= FALSE, style = "height:480px; overflow-y: scroll;overflow-x: scroll;")
      ),
      
      # uiOutput("pd_ui"),
      # column(1, actionButton("pd_submit", "Next"))
      
      # box(status = "primary", width = 12, solidHeader = F, 
      #     h3("The Four Retirement Products"),
      #     h5("1. Purchase an annuity with the inital retirement fund."),
      #     h5("2. Drawdown the retirement fund for the remainder of lifetime."),
      #     h5("3. The starting capital is drawdown for n years. The fund value at the end of that 
      #        that period is used to purchase an annuity"),
      #     h5("4. Upon retirement a deferred annuity, with the same periodic payment as the annuity in the 
      #        first scenario, is purchased. The remainder of the inital reitrement fund is drawdown for the deferred period.")
      #     )
      
      )
  )
)