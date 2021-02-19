list(
  box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(h3("Parameters:"),
                 style = "margin-top:1em",
                 radioButtons("percent_yn", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T)),
                 numericInputIcon(inputId = "retire_age", label = "Age at Retirement:", value = 66, min = 55, max = 105, icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 selectInput("withdraw_freq", "Withdrawal Frequency:", freq_list),
                 numericInputIcon(inputId = "annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "n_sim", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                 actionButton(inputId = "resim", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
    ),
    
    mainPanel(
            div(id = "life_ex_box",box(
              title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
              h3(textOutput('life_ex')))
            ),
            div(id = "average_fund_box",box(
              title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
              h3(textOutput("drawdown_average_fund")))
            ),
            div(id = "prob_ruin_box", box(
              title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
              h3(textOutput("drawdown_ruin_prob")))
            ),
            box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_d"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
            box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot"))
            )
  )
)