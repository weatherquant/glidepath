list(
  box(h1("Sequencing Risk Demonstration"), width = 12, background = "light-blue"),
  
  tabBox(width = 4, height = "520px",
         tabPanel("Initial Inputs",
                             actionButton(inputId = "seq_resim1", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                             br(), br(),
                             numericInputIcon(inputId = "seq_start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro")),
                             awesomeRadio("seq_withdraw_type", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                             div(id = "seq_annual_withdrawals_input", numericInputIcon(inputId = "seq_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 30000, min = 0, icon = icon("euro"))),
                             div(id = "seq_percent_withdrawal_input", numericInputIcon(inputId = "seq_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                             br(),  
                             #numericInputIcon(inputId = "seq_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                             numericInputIcon(inputId = "seq_n_years", label = "Number of Years:", value = 25, min = 0, icon = list(NULL, "Years")),
                             selectInput("seq_withdraw_freq", "Withdrawal Frequency:", freq_list)
                      ),
           
           tabPanel("Assumptions", 
                             actionButton(inputId = "seq_resim2", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                             br(),br(),
                             numericInputIcon(inputId = "seq_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                             numericInputIcon(inputId = "seq_best_annual_return", label = "Highest Annual Return Achieved:", value = 15, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                             br(),
                             numericInputIcon(inputId = "seq_worst_annual_return", label = "Lowest Annual Return Achieved:", value = -5, min = -100, max = 100, icon = list(NULL, icon("percent"))),
                             numericInputIcon(inputId = "seq_annual_inflation", label = "Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent")))
                    )
  ),
  
  tabBox(width = 8, height = "520px",
    tabPanel("Summary",
             plotlyOutput("sequencing_sim_plot_three"),
    ),
    tabPanel("Table", DT::dataTableOutput("seq_drawdown_table"), style = "height:424px"
    )
  ),
  
  box(status = "primary", width = 12, solidHeader = T, title = "What is Sequencing Risk?",
      h4("Sequencing risk is the danger that the timing of withdrawals from a retirement account will have a negative impact on the overall rate of return available to the investor. 
                     This can have a significant effect on a retiree who depends on the income from a lifetime of investing and is no longer contributing new capital that could offset losses."),
      h4("The purpose of the demonstration above is to aid your understanding of the dangers that sequencing risk poses to a savings fund in its decumulation phase.")
      # h4("Above is an interactive demonstration of the impact that the sequence of returns can have on the value of a fund.
      #    The three funds shown have the same mean return, however the fund values at the end of the period vary dramatically.
      #    This is due to the significant exposure funds in the decumulation phase of their journey have to the order of returns.
      #    It is important to be aware of the dangers posed by sequencing risk as good returns in the early years of drawdown plays a large roles
      #    in prolonging the length of time that drawdown can occur."
      #    )
  )
)
