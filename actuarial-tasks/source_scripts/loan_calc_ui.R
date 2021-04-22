list(
        box(h1("Loan Calculator"), width = 12, background = "light-blue"),
        box(
          title = "Parameters", status = "primary", solidHeader = T,
          numericInputIcon(inputId = "loan_inital_balance", label = "Loan Amount:", value = 500000, step = 5000, icon = icon("euro")),
          numericInputIcon(inputId = "loan_interest", label = "Interest Rate:", value = 5, icon = list(NULL, icon("percent"))),
          numericInputIcon(inputId = "loan_years", label = "Term of Loan (in Years):", value = 25, icon = list(NULL, "Years")),
          selectInput("loan_freq", "Frequency of Repayments:", freq_list),
          hr(),
          h4(strong("Periodic Repayment Amount:")),
          h3(textOutput("loan_periodic_repay"))
        ),
        
        box(
          title = "Loan Schedule", status = "primary", solidHeader = T,
          DT::dataTableOutput("loan_schedule"), style = "height:424px"
        ),
        box(title = "Loan Balance Outstanding Over Time", status = "primary", solidHeader = T, height = "500px", style = "margin-top:1em; margin-right:3em", plotOutput("loan_plot_balance")),
        box(title = "Interest vs Capital Proportions per Repayment", status = "primary", solidHeader = T, height = "500px", style = "margin-top:1em; margin-right:3em", plotOutput("loan_plot_interest_vs_capital"))
)  