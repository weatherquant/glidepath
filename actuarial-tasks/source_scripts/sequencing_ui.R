list(
  box(h1("Sequencing Risk Demonstration"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      style = "margin-top:1em",
      numericInputIcon(inputId = "seq_start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
      numericInputIcon(inputId = "seq_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro")),
      #numericInputIcon(inputId = "seq_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      selectInput("seq_withdraw_freq", "Withdrawal Frequency:", freq_list),
      numericInputIcon(inputId = "seq_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "seq_best_annual_return", label = "Highest Annual Return Achieved:", value = 15, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "seq_worst_annual_return", label = "Lowest Annual Return Achieved:", value = -5, min = -100, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "seq_annual_inflation", label = "Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
      numericInputIcon(inputId = "seq_n_years", label = "Number of Years:", value = 25, min = 0, icon = list(NULL, "Years"))
    ),
    
    mainPanel(
      box(title = "Summary", status = "primary", width = 12, solidHeader = T, plotlyOutput("sequencing_sim_plot_three"))
    )
  )
)