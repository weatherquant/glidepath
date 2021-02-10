list(
  box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(h3("Parameters:"),
                 style = "margin-top:1em",
                 numericInputIcon(inputId = "retire_age", label = "Age at Retirement:", value = 66, min = 55, max = 105, icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
                 selectInput("withdraw_freq", "Withdrawal Frequency:", freq_list),
                 numericInputIcon(inputId = "annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "n_sim", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                 actionButton(inputId = "resim", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
    ),
    
    mainPanel(
      box(title = "Probability of Ruin", status = "primary", width = 6, solidHeader = T,
          h3(textOutput("drawdown_ruin_prob"))),
      box(title = "Average Fund Value", status = "primary", width = 6, solidHeader = T,
          h3(textOutput("drawdown_average_fund"))),
      box(title = "Life Expectancy", status = 'primary', width = 6, solidHeader = T,
          h3(textOutput('life_ex'))),
      box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot"))
      
    )
  )
)