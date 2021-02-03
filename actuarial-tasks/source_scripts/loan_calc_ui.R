list(
        box(h1("Loan Calculator"), width = 12, background = "light-blue"),
        box(
          title = "Parameters", status = "primary", solidHeader = T,
          numericInputIcon(inputId = "PV", label = "Loan Amount:", value = 500000, icon = icon("euro")),
          numericInputIcon(inputId = "int", label = "Interest Rate:", value = 5, icon = list(NULL, icon("percent"))),
          numericInputIcon(inputId = "n", label = "Term of Loan (in Years):", value = 25, icon = list(NULL, "Years")),
          selectInput("freq", "Frequency of Repayments:", freq_list),
          hr(),
          h4(strong("Periodic Repayment Amount:")),
          h2(textOutput("repay"))
        ),
        
        box(
          title = "Loan Schedule", status = "primary", solidHeader = T,
          DT::dataTableOutput("loan_schedule"), style = "height:430px; overflow-y: scroll;overflow-x: scroll;"
        ),
        box(title = "Loan Outstanding Over Time", status = "primary", solidHeader = T, height = "500px", plotOutput("loan_balance")),
        box(title = "Interest vs Capital per Payment", status = "primary", solidHeader = T,height = "500px", plotOutput("int_cap"))
)  