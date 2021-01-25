library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(readxl)
#hello
library(lifecontingencies)
options(scipen=999)

# ILT15 Life Tables -------------------------------------------------------
cnames <- read_excel("data/ILT15.xlsx", sheet = 1, n_max = 0) %>%
    names()

life_table_female <- read_xlsx("data/ILT15.xlsx", sheet = 1, skip=1, col_names = cnames) %>% 
    drop_na()

life_table_male <- read_xlsx("data/ILT15.xlsx", sheet = 2, skip=1, col_names = cnames) %>% 
    drop_na()

life_table_female_reduced <- life_table_female %>% 
    select(qx1) %>% 
    transmute(qxnew = qx1 * 0.5)

life_table_male_reduced <- life_table_male %>% 
    select(qx1) %>% 
    transmute(qxnew = qx1 * 0.42)

df_female <- data.frame(x = life_table_female[,1],
                        lx = life_table_female[,2],
                        qx = life_table_female_reduced[,1])

df_male <- data.frame(x = life_table_male[,1],
                      lx = life_table_male[,2],
                      qx = life_table_male_reduced[,1])

x1_female <- df_female[,1]
lx1_female <- df_female[,2]
qx_female <- df_female[,3]

x1_male <- df_male[,1]
lx1_male <- df_male[,2]
qx_male <- df_male[,3]

ILT15_female_reduced <- probs2lifetable(probs=qx_female,radix=100000,"qx",name="ILT15_female_reduced")
ILT15_male_reduced <- probs2lifetable(probs=qx_male,radix=100000,"qx",name="ILT15_male_reduced")
listOfTables <- list(ILT15_female_reduced, ILT15_male_reduced)


# Frequencies -------------------------------------------------------------
freq_list = c("Annually", "Semi-Annually", "Quarterly", "Bi-Monthly", "Monthly", "Fortnightly", "Weekly", "Daily")
p_list = c(1, 2, 4, 6, 12, 26, 52, 365)


# General UI --------------------------------------------------------------
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
                
                # Loan Calculator UI ------------------------------------------------------
                
                tabItem(tabName = 'loan_calc', 
                        
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
                        
                ),
                
                
                # SORP UI -----------------------------------------------------------------
                
                
                
                tabItem(tabName = 'sorps',
                        
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
                                            
                                            tabPanel("Assumptions",
                                                     style = "margin-top:1em",
                                                     numericInputIcon(inputId = "salEsc", label = "Salary Escalation:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                                     numericInputIcon(inputId = "discountRate", label = "Discount Rate from FV to CV:", value = 2.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                                     numericInputIcon(inputId = "iPost", label = "Interest Rate for Annuity:", value = 2, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                                     numericInputIcon(inputId = "annEsc", label = "Annunity Escalation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                                                     numericInputIcon(inputId = "guaranteed", label = "Guaranteed Period (in Years):", value = 5, min = 0, max = 39, icon = list(NULL, "Years")),
                                                     h4(strong("Percentage of Fund Held In:")),
                                                     sliderInput("equity", "Equity/Property:", min = 0, max = 100, value = 40, step = 1),
                                                     sliderInput("fixed", "Fixed Interest Securities:", min = 0, max = 60, value = 30, step = 1),
                                                     sliderInput("cash", "Cash/Other:", min = 0, max = 100, value = 30, step = 1),
                                                     numericInputIcon(inputId = "investCharge", label = "Investment Charges:", value = 0.5, min = 0, max = 100, icon = list(NULL, icon("percent")))
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
                                                     box(title = "Contributions and Fund Value over Time", width = 12, status = "primary", solidHeader = T, DT::dataTableOutput("table"),rownames= FALSE, style = "height:750px; overflow-y: scroll;overflow-x: scroll;"))
                                ),
                                
                            )
                        )
                        
                ),
                
                # Drawdown UI -------------------------------------------------------------
                tabItem(tabName = 'drawdown',
                        
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
                                
                            ),
                            
                        )
                )
                
                # General Server ----------------------------------------------------------
                
                
            )
        )
    )
)
server <- function(input, output, session) {
    
    # Loan Calculator Server --------------------------------------------------
    loanSummary <- reactive(
        {
            PV = input$PV
            int = input$int / 100
            n = input$n
            freq = input$freq
            
            p = p_list[match(input$freq, freq_list)]
            ip_p = effective2Convertible(int, k = p)/p
            
            total_payments = n * p
            repay = PV/annuity(i = int, n = n, k = p)/p
            
            repay_no = seq(0, total_payments)
            repay_vect = c(0, rep(repay, total_payments))
            balance = numeric(total_payments + 1)
            int_paid = numeric(total_payments + 1)
            cap_paid = numeric(total_payments + 1)
            int_percent = numeric(total_payments + 1)
            cap_percent = numeric(total_payments + 1)
            
            balance[1] = PV
            
            for(i in (1:(total_payments))){
                int_paid[i + 1] = ip_p * balance[i]
                cap_paid[i + 1] =  repay - int_paid[i + 1]
                balance[i + 1] = balance[i] - cap_paid[i + 1]
                int_percent[i + 1] = (int_paid[i + 1]) / repay 
                cap_percent[i + 1] = (cap_paid[i + 1]) / repay 
            }
            
            loan_summary <- data.frame(repay_no, balance, int_percent, cap_percent, int_paid, cap_paid, repay_vect)
            return(loan_summary)
        }
    )
    
    output$repay <- renderText({
        loan_summary = loanSummary()
        repayment = loan_summary[2, 7]
        return(c("€", format(round(as.numeric(repayment), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
    })
    
    output$repay_box <- renderInfoBox({
        loan_summary = loanSummary()
        repayment = paste((c("€", format(round(as.numeric(loan_summary[2, 7]), 2), nsmall = 2, big.mark = ",", scientific=FALSE))), collapse = "")
        infoBox(
            "Periodic Repayment Amount", repayment, icon = icon("euro")
        )
    })
    
    output$loan_balance <- renderPlot({
        loan_summary = loanSummary()
        ggplot(loan_summary, aes(x = repay_no, y = balance, fill="#4A8DBF", color="#4A8DBF")) + geom_bar(stat='identity', color = "#4A8DBF", fill = "#4A8DBF") + labs(x = "Repayment Number", y = "Remaining Balance") + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                             panel.background = element_blank(), axis.line = element_line(colour = "black"))
    })
    
    output$int_cap <- renderPlot({
        loan_summary = loanSummary()
        loan_summary[, 3:4] = 100*loan_summary[, 3:4]
        colnames(loan_summary) = c("repay_no", 'balance', "Interest", "Capital", 'int_paid', 'cap_paid', 'repay_vect')
        int_cap <- data.frame(pivot_longer(select(loan_summary, -repay_vect), Interest:Capital, names_to = "int_or_cap", values_to = "value"))
        ggplot(int_cap, aes(x=repay_no, y=value, fill=int_or_cap)) + geom_bar(stat="identity") + scale_fill_manual(values = c("#4A8DBF", "#BF7C4A")) + labs(x = "Repayment Number", y = "Proportion of Repayment", fill = NULL) + scale_x_continuous(limits = c(0, nrow(loan_summary)), expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
    })
    
    output$loan_schedule <- renderDataTable({
        loan_summary = loanSummary()
        loan_summary_tidy <- data.frame(select(loan_summary, -repay_no, -repay_vect), row.names = loan_summary[,1])
        colnames(loan_summary_tidy) = c("Balance", "% Interest", "% Capital", "Interest Paid", "Capital Paid")
        ls <- datatable(loan_summary_tidy, options = list(paging = FALSE)) 
        ls <- formatCurrency(ls, columns = c(1, 4, 5), currency = "€")
        ls <- formatPercentage(ls, columns = c(2, 3), digits = 2)
        return(ls)
    })
    
    
    # SORP Server -------------------------------------------------------------
    SORP <- reactive({
        preK = p_list[match(input$PreK, freq_list)]
        postK = p_list[match(input$PostK, freq_list)]
        
        iPre = ((input$equity/100) * 0.05) + ((input$fixed/100) * 0.025) + ((input$cash/100) * 0.01) - (input$investCharge/100)
        netiPost = ((1 + (input$iPost/100))/(1 + (input$annEsc/100))) - 1
        
        iPreK = effective2Convertible(i=iPre, k=preK)
        iPostK = effective2Convertible(i=(input$iPost/100), k=postK)
        
        fundValueX = numeric((input$age[2] - input$age[1])*preK + 2)
        ages = numeric((input$age[2] - input$age[1])*preK + 2)
        ages_exact = numeric((input$age[2] - input$age[1])*preK + 2)
        periods = numeric((input$age[2] - input$age[1])*preK + 2)
        EEContribution = numeric((input$age[2] - input$age[1])*preK + 2)
        ERContribution = numeric((input$age[2] - input$age[1])*preK + 2)
        totalContribution = numeric((input$age[2] - input$age[1])*preK + 2)
        sorp_vector = numeric((input$age[2] - input$age[1])*preK + 2)
        
        sal = input$sal
        ages[1] = input$age[1] - 1
        ages_exact[1] = input$age[1] - 1/preK
        periods[1] = preK - 1
        fundValueX[1] = input$fundvalue
        
        for (m in 2:((input$age[2] - input$age[1])*preK + 2)){
            ages[m] = ages[m-1]
            ages_exact[m] = ages_exact[m - 1] + 1/preK
            periods[m] = periods[m - 1] + 1
            EEContribution[m] = sal*(input$emp_contri/100)*1/preK
            ERContribution[m] = sal*(input$empr_contri/100)*1/preK
            totalContribution[m] = EEContribution[m] + ERContribution[m]
            fundValueX[m] = fundValueX[m-1]*(1+iPreK/preK) + totalContribution[m]
            
            if((m - 1)%%preK == 0){
                sal = sal * (1 + (input$salEsc/100))
            }
            
            if((m - 2)%%preK == 0){
                periods[m] = 0
                ages[m] = ages[m - 1] + 1
            }
        }
        
        guar_ann = annuity(i = netiPost, n = input$guaranteed, k = postK, type = "advance")
        
        if (input$relationship == 1) {
            SORP_Annuity = guar_ann + axn(ILT15_female_reduced, x = input$age[2], i = netiPost, k = postK, m = input$guaranteed, payment = "advance")
        } else {
            SORP_Annuity = guar_ann + axyzn(listOfTables, x = c(input$age[2], input$age[2]), i = netiPost, m = input$guaranteed, k = postK, status = "last", payment = "advance")
        }
        
        sorp_vector[1] = SORP_Annuity
        
        df_fund <- data.frame(age = ages, period = periods, age_exact = ages_exact,
                              EmployeeContribution = EEContribution,
                              EmployerContribution = ERContribution,
                              TotalContribution = totalContribution,
                              FundValue = fundValueX,
                              SORPAnnuity = sorp_vector
        )
        
        return(df_fund)
    })
    
    output$plot <- renderPlot({
        
        AgeandFundValue <- SORP() %>% select(age_exact, FundValue)
        
        ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
            geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
            labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
            theme(legend.position = "none") +
            scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
    })
    
    output$fundFV <- renderText({
        sorp <- SORP()
        preK = p_list[match(input$PreK, freq_list)]
        fundvalue_at_retirement <- sorp %>% select(FundValue)
        fund_FV <- fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]
        return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
    })
    
    output$pensionPaymentFV <- renderText({
        sorp <- SORP()
        preK = p_list[match(input$PreK, freq_list)]
        postK = p_list[match(input$PostK, freq_list)]
        fundvalue_at_retirement <- sorp %>% select(FundValue)
        periodic_payment_FV = (fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]/sorp[1, 8])/postK
        return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
    })
    
    output$fundCV <- renderText({
        sorp <- SORP()
        preK = p_list[match(input$PreK, freq_list)]
        fundvalue_at_retirement <- sorp %>% select(FundValue)
        discount_factor = 1/((1 + input$discountRate/100)^(input$age[2] - input$age[1]))
        fund_CV = fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1] * discount_factor
        return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
    })
    
    output$pensionPaymentCV <- renderText({
        sorp <- SORP()
        preK = p_list[match(input$PreK, freq_list)]
        postK = p_list[match(input$PostK, freq_list)]
        fundvalue_at_retirement <- sorp %>% select(FundValue)
        discount_factor = 1/((1 + input$discountRate/100)^(input$age[2] - input$age[1]))
        periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]/sorp[1, 8])/postK
        return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
    })
    
    output$table <- renderDataTable({
        sorp = SORP()
        preK = p_list[match(input$PreK, freq_list)]
        if(preK == 1){
            sorp = data.frame(select(sorp, -period, -age_exact, -SORPAnnuity))
            colnames(sorp) = c("Age", "Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period")
        }else{
            sorp = data.frame(select(sorp, -age_exact, -SORPAnnuity))
            colnames(sorp) = c("Age", "Period", "Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period")
        }
        sorp <- datatable(sorp, options = list(paging = FALSE), rownames= FALSE)
        sorp <- formatCurrency(sorp, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period"), currency = "€")
        return(sorp)
    })
    
    observeEvent(input$equity,{
        updateSliderInput(session, "fixed", max = 100 - input$equity)
        disable("cash") # putting this here keeps the slider disabled all the time (but still shows updating)
    })
    
    observe({
        updateSliderInput(session, "cash", value = 100 - input$equity - input$fixed)
    })
    
    observeEvent(input$age[2],{
        updateNumericInputIcon(session, "guaranteed", max = 105 - input$age[2])
    })
    
    
    # Drawdown Server ---------------------------------------------------------
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
        n.years = input$n_years
        
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
    
    output$drawdown_sim_plot <- renderPlot({
        
        Spaths <- Drawdown_Sim()
        dat <- vector("list", input$n_sim)
        p <- ggplot()
        for (i in seq(input$n_sim)) {
            dat[[i]] <- data.frame(time = (1:((p_list[match(input$withdraw_freq, freq_list)] * input$n_years) +1)), capital = Spaths[i,])
            p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
        } 
        return(p)
        
    })
}

# Run the application 
shinyApp(ui, server)