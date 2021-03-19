list(
  box(h1("Drawdown Products"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Common Parameters",
                           style = "margin-top:1em",
                           actionButton(inputId = "pd_resim1", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                           br(), br(),
                           sliderInput("pd_age", "Drawdown Age and Annuity Age:", value = c(66, 76), min = 55, max = getOmega(ILT15_female_reduced), step = 1),
                           awesomeRadio("pd_relationship", "Relationship Status:", choices = list("Single" = 1, "Married" = 2), inline = TRUE),
                           selectInput("pd_withdraw_freq", "Withdrawal Frequency:", freq_list_drawdown),
                           numericInputIcon(inputId = "pd_starting_capital", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                           ),

                  tabPanel("Withdrawal Parameters",
                           # style = "margin-top:1em",
                           actionButton(inputId = "pd_resim2", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                           br(), br(),
                           h4(strong("100% Drawdown:")),
                           awesomeRadio("pd_withdraw_type_drawdown", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           div(id = "pd_annual_withdrawals_drawdown_input", numericInputIcon(inputId = "pd_annual_withdrawals_drawdown", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro"))),
                           div(id = "pd_percent_withdrawal_drawdown_input", numericInputIcon(inputId = "pd_percent_withdrawal_drawdown", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                           hr(),
                           h4(strong("Drawdown and Subsequent Annuity Purchase:")),
                           awesomeRadio("pd_withdraw_type_buylater", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           div(id = "pd_annual_withdrawals_buylater_input", numericInputIcon(inputId = "pd_annual_withdrawals_buylater", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro"))),
                           div(id = "pd_percent_withdrawal_buylater_input", numericInputIcon(inputId = "pd_percent_withdrawal_buylater", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                           hr(),
                           h4(strong("Deferred Annuity and Drawdown:")),
                           style = "margin-top:1em",
                           awesomeRadio("pd_withdraw_type_deferred", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                           div(id = "pd_annual_withdrawals_deferred_input", numericInputIcon(inputId = "pd_annual_withdrawals_deferred", label = "Total Withdrawals per Annum:", value = 15000, min = 0, icon = icon("euro"))),
                           div(id = "pd_percent_withdrawal_deferred_input", numericInputIcon(inputId = "pd_percent_withdrawal_deferred", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                           ),

                  tabPanel("Market Parameters",
                          style = "margin-top:1em",
                          actionButton(inputId = "pd_resim3", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                          br(), br(),
                          h5(strong("Portfolio Suggestion Tool:")),
                          actionButton(inputId = "pd_surveydisplay", label = "Risk Profiler", style = "background-color: white", icon("poll")),
                          bsTooltip("pd_surveydisplay", "Complete the survey to identify a suitable level of risk",
                                    "right", options = list(container = "body")),
                          h6(textOutput("pd_save_results_text")),
                          numericInputIcon(inputId = "pd_annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "pd_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "pd_annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          numericInputIcon(inputId = "pd_annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                          )
      )),
    
    mainPanel(
      uiOutput("pd_ui"),
      column(1, actionButton("pd_submit", "Next"))
      )
  )
)