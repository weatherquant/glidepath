list(
  box(h1("SORP Calculator"), width = 12, background = "light-blue"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Parameters",
                           style = "margin-top:1em",
                           sliderInput("age", "Current Age and Retirement Age:", value = c(45,66), min = 16, max = 105),
                           radioButtons("relationship", "Relationship Status:", choices = list("Single" = 1, "Married" = 2)),
                           numericInputIcon(inputId = "sal", label = "Current Salary:", value = 50000, min = 0, icon = icon("euro")),
                           numericInputIcon(inputId = "fundvalue", label = "Current Fund Value:", value = 100000, min = 0, icon = icon("euro")),
                           selectInput("PreK", "Contribution Frequency:", freq_list),
                           selectInput("PostK", "Annuity Payment Frequency:", freq_list),
                           numericInputIcon(inputId = "emp_contri", label = "Employee Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "empr_contri", label = "Employer Contribution Percentage:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                  ),
                  
                  #Note: any inputs here must be included in the server code for the reset button
                  tabPanel("Assumptions",
                           style = "margin-top:1em",
                           numericInputIcon(inputId = "salEsc", label = "Salary Escalation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "discountRate", label = "Discount Rate from FV to CV:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "iPost", label = "Interest Rate for Annuity:", value = 2, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "annEsc", label = "Annunity Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           numericInputIcon(inputId = "guaranteed", label = "Guarantee Period:", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
                           h4(strong("Percentage of Fund Held In:")),
                           sliderInput("equity", "Equity/Property:", min = 0, max = 100, value = 40, step = 1),
                           sliderInput("fixed", "Fixed Interest Securities:", min = 0, max = 60, value = 30, step = 1),
                           sliderInput("cash", "Cash/Other:", min = 0, max = 100, value = 30, step = 1),
                           numericInputIcon(inputId = "investCharge", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                           actionButton(inputId = "default", label = "Reset to Default", style = "background-color: white")
                  )
      ),
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           style = "margin-top:1em",
                           box(title = "Future Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundFV")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentFV"))
                           ),
                           box(title = "Current Values", status = "primary", solidHeader = T,
                               h4("Fund Value At Retirement:"),
                               h3(textOutput("fundCV")),
                               hr(),
                               h4("Periodic Pension Payment:"),
                               h3(textOutput("pensionPaymentCV"))
                           ),
                           box(title = "Accumulated Wealth", width = 12, status = "primary", solidHeader = T, plotOutput("plot"))
                  ),
                  
                  tabPanel("Table",
                           style = "margin-top:1em",
                           box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table"),rownames= FALSE, style = "height:750px; overflow-y: scroll;overflow-x: scroll;")
                           )
                  )
            )
      )
)