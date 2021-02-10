list(
  SORP_react <- reactive({
    return(SORP(input$age[1], input$age[2], input$relationship, input$sal, input$fundvalue, input$PreK, input$PostK, input$emp_contri, input$empr_contri, input$salEsc, input$iPost, input$annEsc, input$guaranteed, input$equity, input$fixed, input$cash, input$investCharge))
    }),
  
  observeEvent(input$default, {
    updateNumericInputIcon(session, "salEsc", value = 1.5)
    updateNumericInputIcon(session, "discountRate", value = 2.5)
    updateNumericInputIcon(session, "iPost", value = 0.5)
    updateNumericInputIcon(session, "annEsc", value = 1)
    updateNumericInputIcon(session, "guaranteed", value = 5)
    updateSliderInput(session, "equity", value = 40)
    updateSliderInput(session, "fixed", value = 30)
    updateSliderInput(session, "cash", value = 30)
    updateNumericInputIcon(session, "investCharge", value = 0.5)
  }),
  
  output$plot <- renderPlot({
    
    AgeandFundValue <- SORP_react() %>% select(age_exact, FundValue)
    
    ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }),
  
  output$fundFV <- renderText({
    sorp <- SORP_react()
    preK = p_list[match(input$PreK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    fund_FV <- fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]
    return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentFV <- renderText({
    sorp <- SORP_react()
    preK = p_list[match(input$PreK, freq_list)]
    postK = p_list[match(input$PostK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    periodic_payment_FV = (fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$fundCV <- renderText({
    sorp <- SORP_react()
    preK = p_list[match(input$PreK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate/100)^(input$age[2] - input$age[1]))
    fund_CV = fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1] * discount_factor
    return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentCV <- renderText({
    sorp <- SORP_react()
    preK = p_list[match(input$PreK, freq_list)]
    postK = p_list[match(input$PostK, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate/100)^(input$age[2] - input$age[1]))
    periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age[2] - input$age[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$table <- renderDataTable({
    sorp = SORP_react()
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