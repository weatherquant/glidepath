list(
  box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(h3("Parameters:"),
                 style = "margin-top:1em",
                 numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
                 selectInput("withdraw_freq", "Withdrawal Frequency:", freq_list),
                 numericInputIcon(inputId = "annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "n_sim", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                 numericInputIcon(inputId = "n_years", label = "Time to Run (in Years):", value = 25, min = 0, max = 39, icon = list(NULL, "Years"))
    ),
    
    mainPanel(
      box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot"))
      
    )
  )
)