list(
  box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(h3("Parameters:"),
                 style = "margin-top:1em",
                 numericInputIcon(inputId = "drawdown_retire_age", label = "Age at Retirement:", value = 66, min = 55, max = getOmega(ILT15_female_reduced), icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "drawdown_start_capital", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                 selectInput("drawdown_withdraw_freq", "Withdrawal Frequency:", freq_list_drawdown),
                 awesomeRadio("drawdown_withdraw_type", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                 numericInputIcon(inputId = "drawdown_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "drawdown_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 actionButton(inputId = "drawdown_resim", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
    ),
    
    mainPanel(
            box(
              title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
              h3(textOutput('drawdown_text_life_ex'))
            ),
            box(
              title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
              h3(textOutput("drawdown_text_average_fund_life_ex"))
            ),
            box(
              title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
              h3(textOutput("drawdown_text_ruin_prob_life_ex"))
            ),
            box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("drawdown_table"), rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
            box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_plot_sims")),
            box(title = "Drawdown Percentile Plot", status = "primary", width = 12, solidHeader = T, plotlyOutput("drawdown_plot_percentiles"))
            )
  )
)