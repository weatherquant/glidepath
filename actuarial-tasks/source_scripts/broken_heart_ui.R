list(
  box(h1("Broken Heart Simulator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(h3("Parameters:"),
                 style = "margin-top:1em",
                 awesomeRadio("gender_bh", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
                 numericInputIcon(inputId = "age_bh", label = "Current Age:", value = 66, min = 66, max = 95, icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "widowed_bh", label = "Age Widowed:", value = 66, min = 66, max = 95, icon = list(NULL, " Years")),
                 numericInputIcon(inputId = "start_capital_bh", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                 radioButtons("percent_yn_bh", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T)),
                 numericInputIcon(inputId = "annual_withdrawals_bh", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro")),
                 numericInputIcon(inputId = "percent_withdrawal_bh", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 selectInput("withdraw_freq_bh", "Withdrawal Frequency:", freq_list),
                 numericInputIcon(inputId = "annual_mean_return_bh", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_ret_std_dev_bh", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inflation_bh", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "annual_inf_std_dev_bh", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "n_sim_bh", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                 actionButton(inputId = "resim_bh", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
    ),
    
    mainPanel(
      
      div(id = "life_ex_diff_bh",box(
        title = "Difference in Life Expectancy", status = 'primary', solidHeader = T, width = 7,
        h3(textOutput('life_ex_diff_bh')))
      ),
      
      box(title = "Not Widowed", status = "primary", solidHeader = T,
          h4("Life Expectancy"),
          h3(textOutput('life_ex_no_bh')),
          hr(),
          h4("Average Final Fund Value:"),
          h3(textOutput("drawdown_average_fund_no_bh")),
          hr(),
          h4("Probability of Ruin"),
          h3(textOutput('drawdown_ruin_prob_no_bh')),
          hr(),
          h4("Total Drawdwon Withdrawals"),
          h3(textOutput('drawdown_total_withdrawal_no_bh')),
          
          ),
      
      box(title = "Widowed", status = "primary", solidHeader = T,
          h4("Life Expectancy"),
          h3(textOutput("life_ex_bh")),
          hr(),
          h4("Average Final Fund Value:"),
          h3(textOutput("drawdown_average_fund_bh")),
          hr(),
          h4("Probability of Ruin"),
          h3(textOutput("drawdown_ruin_prob_bh")),
          hr(),
          h4("Total Drawdwon Withdrawals"),
          h3(textOutput('drawdown_total_withdrawal_bh')),
      ),

      
      
      # div(id = "life_ex_box_bh",box(
      #   title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
      #   h3(textOutput('life_ex_bh')))
      # ),
      # div(id = "life_ex_box_no_bh",box(
      #   title = "Life Expectancy (If Not Widowed)", status = 'primary', solidHeader = T, width = 4,
      #   h3(textOutput('life_ex_no_bh')))
      # ),
      # div(id = "life_ex_box_diff_bh",box(
      #   title = "Difference in Life Expectancy", status = 'primary', solidHeader = T, width = 4,
      #   h3(textOutput('life_ex_diff_bh')))
      # ),
      # div(id = "average_fund_box_bh",box(
      #   title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
      #   h3(textOutput("drawdown_average_fund_bh")))
      # ),
      #div(id = "average_fund_box_no_bh",box(
        #   title = "Average Fund Value", status = "primary", solidHeader = T, width = 4,
        #   h3(textOutput("drawdown_average_fund_no_bh")))
        # ),
      # div(id = "prob_ruin_box_bh", box(
      #   title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
      #   h3(textOutput("drawdown_ruin_prob_bh")))
      # ),
      # div(id = "prob_ruin_box_no_bh", box(
      #   title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
      #   h3(textOutput("drawdown_ruin_prob_no_bh")))
      # ),
      box(title = "Table", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table_d_bh"),rownames= FALSE, style = "height:400px; overflow-y: scroll;overflow-x: scroll;"),
      tabsetPanel(type = 'tabs',
        tabPanel("Widowed vs Not-Widowed Death Probabilites", style = "margin-top:1em", box(title = "Comparison of Widowed vs Non-Widowed Death Probabilities", status = "primary", width = 12, solidHeader = T, plotlyOutput("qx_change_plot_bh"))),
        tabPanel("Short-Term Effect of Widowhood on Mortality", style = "margin-top:1em", box(title = "Short-Term Effect of Widowhood on the Probability of Death", status = "primary", width = 12, solidHeader = T, plotlyOutput("life_ex_change_plot_bh"))),
        tabPanel("Drawdown Simulations", style = "margin-top:1em", box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot_bh")))
      )
    )
  )
)