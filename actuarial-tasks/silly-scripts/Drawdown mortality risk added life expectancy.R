library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(readxl)

library(lifecontingencies)
options(scipen=999)

# ILT15 Life Tables -------------------------------------------------------
cnames <- read_excel("C:\\Users\\gmcsw\\Downloads\\ILT15.xlsx", sheet = 1, n_max = 0) %>%
  names()

life_table_female <- read_xlsx("C:\\Users\\gmcsw\\Downloads\\ILT15.xlsx", sheet = 1, skip=1, col_names = cnames) %>% 
  drop_na()

life_table_male <- read_xlsx("C:\\Users\\gmcsw\\Downloads\\ILT15.xlsx", sheet = 2, skip=1, col_names = cnames) %>% 
  drop_na()

qx_female <- unlist(life_table_female[,5] * 0.5)
qx_male <- unlist(life_table_male[,5] * 0.42)

ILT15_female_reduced <- probs2lifetable(probs=qx_female,radix=100000,"qx",name="ILT15_female_reduced")
ILT15_male_reduced <- probs2lifetable(probs=qx_male,radix=100000,"qx",name="ILT15_male_reduced")
listOfTables <- list(ILT15_female_reduced, ILT15_male_reduced)

#ex_female <- unlist(life_table_female[,8])
#ex_male <- unlist(life_table_male[,8])


# Frequencies -------------------------------------------------------------
freq_list = c("Annually", "Semi-Annually", "Quarterly", "Bi-Monthly", "Monthly", "Fortnightly", "Weekly", "Daily")
p_list = c(1, 2, 4, 6, 12, 26, 52, 365)

ui <- dashboardPage(
  dashboardHeader(title = "Actuarial Tasks in R"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Loan Calculator", tabName = "loan_calc"),
      menuItem("SORP Calculator", tabName = "sorps"),
      menuItem("Drawdown Simulator", tabName = "drawdown")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    fluidRow(
      tabItems(
        tabItem(tabName = 'drawdown',
                
                box(h1("Drawdown Simulator"), width = 12, background = "light-blue"),
                
                sidebarLayout(
                  
                  sidebarPanel(h3("Parameters:"),
                               style = "margin-top:1em",
                               numericInput(inputId = "retire_age", label = "Age at Retirement", value = 66, min = 55, max = 100),
                               numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 200000, min = 0, icon = icon("euro")),
                               #numericInputIcon(inputId = "fund_death", label = "Fund Value at Death", value = 100000, min = 0, icon = icon("euro")),
                               numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 7000, min = 0, step = 100, icon = icon("euro")),
                               selectInput("withdraw_freq", "Withdrawal Frequency:", freq_list),
                               numericInputIcon(inputId = "annual_mean_return", label = "Mean Annual Return:", value = 5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                               numericInputIcon(inputId = "annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 7, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                               numericInputIcon(inputId = "annual_inflation", label = "Mean Annual Inflation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                               numericInputIcon(inputId = "annual_inf_std_dev", label = "Standard Inflation of Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                               numericInputIcon(inputId = "n_sim", label = "Number of Simulations:", value = 25, min = 0, icon = list(NULL, "Simulations")),
                               #numericInputIcon(inputId = "n_years", label = "Time to Run/Life Expectancy (in Years):", value = exn(ILT15_female_reduced, input$retire_age), min = 0, max = 39, icon = list(NULL, "Years")),
                               actionButton(inputId = "resim", label = "Re-Run Simulation", style = "background-color: white", icon("random"))
                  ),
                  
                  mainPanel(
                    box(title = "Probability of Ruin", status = "primary", width = 6, solidHeader = T,
                        h3(textOutput("drawdown_ruin_prob"))),
                    box(title = "Final Fund Value", status = "primary", width = 6, solidHeader = T,
                        h3(textOutput("drawdown_average_final_fund"))),
                    box(title = "Time to Run/Life Expectancy (in years)", status = "primary", width = 6, solidHeader = T,
                        h3(textOutput("life_ex"))),
                    box(title = "Drawdown Simulations", status = "primary", width = 12, solidHeader = T, plotOutput("drawdown_sim_plot"))
                    
                  ),
                  
                )
        )
        
        # General Server ----------------------------------------------------------
        
        
      )
    )
  )
)
server <- function(input, output, session) {
  Drawdown_Sim <- reactive({
    
    #-------------------------------------
    #Assignment
    #-------------------------------------
    
    p = p_list[match(input$withdraw_freq, freq_list)]
    start.capital = input$start_capital
    
    # Investment
    annual.mean.return = input$annual_mean_return / 100
    annual.ret.std.dev = input$annual_ret_std_dev / 100
    
    # Inflation
    annual.inflation = input$annual_inflation / 100
    annual.inf.std.dev = input$annual_inf_std_dev / 100
    
    # Withdrawals
    periodic.withdrawls = input$annual_withdrawals / p
    
    # Number of observations (in Years)
    n.years = exn(ILT15_female_reduced, input$retire_age)
    
    # Number of simulations
    n.sim = input$n_sim
    
    # number of periods to simulate
    n.obs = p * n.years
    
    # periodic Investment and Inflation assumptions
    periodic.mean.return = effective2Convertible(i=annual.mean.return,k=p)/p
    periodic.ret.std.dev = annual.ret.std.dev / sqrt(p)
    
    periodic.inflation = effective2Convertible(i=annual.inflation,k=p)/p
    periodic.inf.std.dev = annual.inf.std.dev / sqrt(p)
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    Spaths = matrix(0, n.sim, n.obs+1)
    Spaths[,1] = start.capital
    
    periodic.invest.returns = numeric(n.obs)
    periodic.inflation.returns = numeric(n.obs)
    
    for(i in 1:n.sim){
      
      periodic.invest.returns = rnorm(n.obs, mean = periodic.mean.return, sd = periodic.ret.std.dev)
      periodic.inflation.returns = rnorm(n.obs, mean = periodic.inflation, sd = periodic.inf.std.dev)
      if (Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawls <= 0) {break}
      Spaths[i,2] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawls
      
      for(j in 2:n.obs){
        if (Spaths[i,j] <= 0) {
          break
        } else if (Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawls <= 0) {
          break
        } else {
          Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawls
        }
      }
    }
    
    return(Spaths)
  })
  
  output$drawdown_ruin_prob <- renderText({
    Spaths <- Drawdown_Sim()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$retire_age)
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / input$n_sim
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  })
  
  output$drawdown_average_final_fund <- renderText({
    Spaths <- Drawdown_Sim()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$retire_age)
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  })
  
  #a table! 
  
  #output$drawdown_withdrawal <- renderText({
   # Spaths <- Drawdown_Sim()
    #p = p_list[match(input$withdraw_freq, freq_list)]
    #n.obs =  p * input$n_years
    #optimal_withdrawal = (mean(Spaths[,(p*round(ex_female[input$retire_age + 1]))]) - input$fund_death) /input$n_years
    #return(c("€", format(round(as.numeric(optimal_withdrawal), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  #})
  
  output$life_ex <- renderText({
    ex = exn(ILT15_female_reduced, input$retire_age)
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  })
  
  output$drawdown_sim_plot <- renderPlot({
    Spaths <- Drawdown_Sim()
    dat <- vector("list", input$n_sim)
    p <- ggplot()
    for (i in seq(input$n_sim)) {
      dat[[i]] <- data.frame(time = (1:((p_list[match(input$withdraw_freq, freq_list)] * exn(ILT15_female_reduced, input$retire_age)) +1)), capital = Spaths[i,])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
    } 
    return(p)
  })
  
  observeEvent(input$resim, {
    updateNumericInputIcon(session, "start_capital", value = input$start_capital + 1)
    updateNumericInputIcon(session, "start_capital", value = input$start_capital)
  })
}
shinyApp(ui = ui, server = server)
