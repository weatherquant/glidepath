list(
  box(h1("Broken Heart Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(h3("Parameters:"),
                 style = "margin-top:1em",
                 awesomeRadio("bh_gender", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
                 numericInputIcon(inputId = "bh_age_current", label = "Current Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "bh_age_widowed", label = "Widowed Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "bh_start_capital", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                 selectInput("bh_withdraw_freq", "Withdrawal Frequency:", freq_list),
                 awesomeRadio("bh_withdraw_type", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                 numericInputIcon(inputId = "bh_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "bh_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "bh_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "bh_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "bh_annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "bh_annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 actionButton(inputId = "bh_resim", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
    ),
    
    mainPanel(
      box(
        title = "Difference in Life Expectancy", status = 'primary', solidHeader = T, width = 7,
        h3(textOutput('bh_text_life_ex_diff'))
      ),
      
      box(title = "Not Widowed", status = "primary", solidHeader = T,
          h4("Life Expectancy"),
          h3(textOutput('bh_text_life_ex_not_widowed')),
          hr(),
          h4("Average Final Fund Value:"),
          h3(textOutput("bh_text_average_fund_life_ex_not_widowed")),
          hr(),
          h4("Probability of Ruin"),
          h3(textOutput('bh_text_ruin_prob_life_ex_not_widowed')),
          hr(),
          h4("Total Drawdown Withdrawals"),
          h3(textOutput('bh_text_total_withdrawals_life_ex_not_widowed')),
          ),
      
      box(title = "Widowed", status = "primary", solidHeader = T,
          h4("Life Expectancy"),
          h3(textOutput("bh_text_life_ex_widowed")),
          hr(),
          h4("Average Final Fund Value:"),
          h3(textOutput("bh_text_average_fund_life_ex_widowed")),
          hr(),
          h4("Probability of Ruin"),
          h3(textOutput("bh_text_ruin_prob_life_ex_widowed")),
          hr(),
          h4("Total Drawdown Withdrawals"),
          h3(textOutput('bh_text_total_withdrawals_life_ex_widowed')),
      ),

      box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("bh_table"), rownames = FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
      tabsetPanel(type = 'tabs',
        tabPanel("Widowed vs Not-Widowed Death Probabilites", style = "margin-top:1em", box(title = "Comparison of Widowed vs Non-Widowed Death Probabilities", status = "primary", width = 12, solidHeader = T, plotlyOutput("bh_qx_change_plot"))),
        tabPanel("Short-Term Effect of Widowhood on Mortality", style = "margin-top:1em", box(title = "Short-Term Effect of Widowhood on the Probability of Death", status = "primary", width = 12, solidHeader = T, plotlyOutput("bh_life_ex_change_plot"))),
        tabPanel("Drawdown Simulations", style = "margin-top:1em", box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("bh_plot_sims")))
      )
    )
  )
)