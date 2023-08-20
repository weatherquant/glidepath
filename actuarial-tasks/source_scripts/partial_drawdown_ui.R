list(
  column(12, box(h1("Retirement Strategies"), width = 12, background = "light-blue")),
  
  column(4,
         box(width = 12,
             tabsetPanel(type = "tabs",
                  tabPanel("Common Parameters",
                           style = "margin-top:1em",
                           actionButton(inputId = "pd_resim1", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                           br(), br(),
                           sliderInput("pd_age", "Drawdown Age and Annuity Age:", value = c(66, 76), min = 55, max = getOmega(ILT15_female_reduced), step = 1),
                           awesomeRadio("pd_relationship", "Relationship Status:", choices = list("Single" = 1, "Married" = 2), inline = TRUE),
                           selectInput("pd_withdraw_freq", "Withdrawal Frequency:", freq_list_drawdown),
                           numericInputIcon(inputId = "pd_starting_capital", label = "Starting Capital:", value = 300000, min = 0, icon = icon("dollar")),
                           numericInputIcon(inputId = "pd_annual_mean_return", label = "Mean Annual Return:", value = 3, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "pd_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "pd_annual_inflation", label = "Mean Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "pd_annual_inf_std_dev", label = "Standard Deviation of Annual Inflation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           ),

                  tabPanel("Withdrawal Parameters",
                           # style = "margin-top:1em",
                           actionButton(inputId = "pd_resim2", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                           br(), br(),
                           h4(strong("100% Drawdown:")),
                           awesomeRadio("pd_withdraw_type_drawdown", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           div(id = "pd_annual_withdrawals_drawdown_input", numericInputIcon(inputId = "pd_annual_withdrawals_drawdown", label = "Total Withdrawals per Annum:", value = 10000, min = 0, icon = icon("dollar"))),
                           div(id = "pd_percent_withdrawal_drawdown_input", numericInputIcon(inputId = "pd_percent_withdrawal_drawdown", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                           hr(),
                           h4(strong("Drawdown and Annuity Purchase:")),
                           awesomeRadio("pd_withdraw_type_buylater", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           div(id = "pd_annual_withdrawals_buylater_input", numericInputIcon(inputId = "pd_annual_withdrawals_buylater", label = "Total Withdrawals per Annum:", value = 10000, min = 0, icon = icon("dollar"))),
                           div(id = "pd_percent_withdrawal_buylater_input", numericInputIcon(inputId = "pd_percent_withdrawal_buylater", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                           hr(),
                           h4(strong("Drawdown and Deferred Annuity:")),
                           style = "margin-top:1em",
                           awesomeRadio("pd_withdraw_type_deferred", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           div(id = "pd_annual_withdrawals_deferred_input", numericInputIcon(inputId = "pd_annual_withdrawals_deferred", label = "Total Withdrawals per Annum:", value = 10000, min = 0, icon = icon("dollar"))),
                           div(id = "pd_percent_withdrawal_deferred_input", numericInputIcon(inputId = "pd_percent_withdrawal_deferred", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))))
                           )
                  )
      )
  ),
  
  column(8,
      box(title = "The Four Retirement Strategies", status = "primary", width = 12, solidHeader = T, 
          h5(strong("100% Annuity:"), "An annuity is purchased with the retirement fund."),
          h5(strong("100% Drawdown:"), "The retirement fund is drawn down for remainder of retiree's life."),
          h5(strong("Drawdown and Annuity Purchase:"), "The initial fund is drawn down for a specified period. The resulting fund value is used to purchase an annuity."),
          h5(strong("Drawdown and Deferred Annuity:"), "A deferred annuity is purchased at retirement. It gives the same payment as the 100% Annuity strategy. The remainder of the retirement fund is drawn down for the deferment period.")
      ),
      
      box(title = "100% Annuity", status = "primary", solidHeader = T,
          h4("Periodic Annuity Payment:"),
          h3(textOutput("pd_text_annuity_payment")),
          hr(),
          h4("Total Annuity Payments Received:"),
          h3(textOutput("pd_text_annuity_cumulative_life_ex"))
      ),
      
      box(title = "100% Drawdown", status = "primary", solidHeader = T,
          h4("Mean Payments Received:"),
          h3(textOutput("pd_text_drawdown_mean_withdrawals_life_ex")),
          hr(),
          h4("Mean Final Fund Value:"),
          h3(textOutput("pd_text_drawdown_mean_fund_life_ex"))
      ),
      
      box(title = "Drawdown and Annuity Purchase", status = "primary", solidHeader = T, 
          h4("Mean Fund Value After Drawdown:"),
          h3(textOutput("pd_text_buylater_mean_fund_end")),
          hr(),
          h4("Periodic Annuity Payment:"),
          h3(textOutput("pd_text_mean_annuity_payment_buylater"))
      ),
      
      box(title = "Drawdown and Deferred Annuity", status = "primary", solidHeader = T,
          h4("Cost of Deferred Annuity:"),
          h3(textOutput("pd_text_annuity_cost_deferred")),
          hr(),
          h4("Periodic Annuity Payment:"),
          h3(textOutput("pd_text_annuity_payment_deferred"))
      )
  ),
  
  column(12,
      tabBox(type = "tabs", width = 12,
             tabPanel("Table", DT::dataTableOutput("pd_comparison_table")),
             tabPanel("Graph of Retirement Income", plotlyOutput("pd_plot_income_compare"))
      ))
)