list(
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
    
    #annuities will increase by 0.33% per annum compound
    retire_year = as.numeric(format(Sys.Date(), "%Y")) + (input$age[2] - input$age[1])
    ann_inc_rate = 1.0033^(retire_year - 2013)
    
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
      SORP_Annuity = (guar_ann + axn(ILT15_female_reduced, x = input$age[2], i = netiPost, k = postK, m = input$guaranteed, payment = "advance"))*ann_inc_rate
    } else {
      SORP_Annuity = (guar_ann + axyzn(listOfTables, x = c(input$age[2], input$age[2]), i = netiPost, m = input$guaranteed, k = postK, status = "last", payment = "advance"))*ann_inc_rate
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
  }),
  
  observeEvent(input$default, {
    updateNumericInputIcon(session, "salEsc", value = 2.5)
    updateNumericInputIcon(session, "discountRate", value = 2.5)
    updateNumericInputIcon(session, "iPost", value = 2)
    updateNumericInputIcon(session, "annEsc", value = 1.5)
    updateNumericInputIcon(session, "guaranteed", value = 5)
    updateSliderInput(session, "equity", value = 40)
    updateSliderInput(session, "fixed", value = 30)
    updateSliderInput(session, "cash", value = 30)
    updateNumericInputIcon(session, "investCharge", value = 0.5)
  }),
  
  output$plot <- renderPlot({
    
    AgeandFundValue <- SORP() %>% select(age_exact, FundValue)
    
    ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }),
  
  output$fundFV <- renderText({
    sorp <- SORP()
    preK = p_list[match(input$PreK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    fund_FV <- fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]
    return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentFV <- renderText({
    sorp <- SORP()
    preK = p_list[match(input$PreK, freq_list)]
    postK = p_list[match(input$PostK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    periodic_payment_FV = (fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$fundCV <- renderText({
    sorp <- SORP()
    preK = p_list[match(input$PreK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate/100)^(input$age[2] - input$age[1]))
    fund_CV = fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1] * discount_factor
    return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentCV <- renderText({
    sorp <- SORP()
    preK = p_list[match(input$PreK, freq_list)]
    postK = p_list[match(input$PostK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate/100)^(input$age[2] - input$age[1]))
    periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
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
  }),
  
  observeEvent(input$equity,{
    updateSliderInput(session, "fixed", max = 100 - input$equity)
    disable("cash") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),
  
  observe({
    updateSliderInput(session, "cash", value = 100 - input$equity - input$fixed)
  }),
  
  observeEvent(input$age[2],{
    updateNumericInputIcon(session, "guaranteed", max = 105 - input$age[2])
  })
)