list(
  SORP_react_sd <- reactive({
    return(SORP(input$age_sd[1], input$age_sd[2], input$relationship_sd, input$sal_sd, input$fundvalue_sd, input$PreK_sd, input$PostK_sd, input$emp_contri_sd, input$empr_contri_sd, input$salEsc_sd, input$iPost_sd, input$annEsc_sd, input$guaranteed_sd, input$equity_sd, input$fixed_sd, input$cash_sd, input$investCharge_sd))
  }),
  
  observeEvent(input$default_sd, {
    updateNumericInputIcon(session, "salEsc_sd", value = 1.5)
    updateNumericInputIcon(session, "discountRate_sd", value = 2.5)
    updateNumericInputIcon(session, "iPost_sd", value = 0.5)
    updateNumericInputIcon(session, "annEsc_sd", value = 1)
    updateNumericInputIcon(session, "guaranteed_sd", value = 5)
    updateSliderInput(session, "equity_sd", value = 40)
    updateSliderInput(session, "fixed_sd", value = 30)
    updateSliderInput(session, "cash_sd", value = 30)
    updateNumericInputIcon(session, "investCharge_sd", value = 0.5)
  }),
  
  output$plot_sd <- renderPlot({
    
    AgeandFundValue <- SORP_react_sd() %>% select(age_exact, FundValue)
    
    ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }),
  
  output$fundFV_sd <- renderText({
    sorp <- SORP_react_sd()
    preK = p_list[match(input$PreK_sd, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    fund_FV <- fundvalue_at_retirement[(input$age_sd[2] - input$age_sd[1])*preK + 1, 1]
    return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentFV_sd <- renderText({
    sorp <- SORP_react_sd()
    preK = p_list[match(input$PreK_sd, freq_list)]
    postK = p_list[match(input$PostK_sd, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    periodic_payment_FV = (fundvalue_at_retirement[(input$age_sd[2] - input$age_sd[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$fundCV_sd <- renderText({
    sorp <- SORP_react_sd()
    preK = p_list[match(input$PreK_sd, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_sd/100)^(input$age_sd[2] - input$age_sd[1]))
    fund_CV = fundvalue_at_retirement[(input$age_sd[2] - input$age_sd[1])*preK + 1, 1] * discount_factor
    return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentCV_sd <- renderText({
    sorp <- SORP_react_sd()
    preK = p_list[match(input$PreK_sd, freq_list)]
    postK = p_list[match(input$PostK_sd, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_sd/100)^(input$age_sd[2] - input$age_sd[1]))
    periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age_sd[2] - input$age_sd[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$table_sd <- renderDataTable({
    sorp = SORP_react_sd()
    preK = p_list[match(input$PreK_sd, freq_list)]
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
  
  observeEvent(input$equity_sd,{
    updateSliderInput(session, "fixed_sd", max = 100 - input$equity_sd)
    disable("cash_sd") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),
  
  observe({
    updateSliderInput(session, "cash_sd", value = 100 - input$equity_sd - input$fixed_sd)
  }),
  
  observeEvent(input$age_sd[2],{
    updateNumericInputIcon(session, "guaranteed_sd", max = 105 - input$age_sd[2])
  }),
  
  drawdown_react_sd <- reactive({
    sorp <- SORP_react_sd()
    start_capital_sd = sorp[length(sorp[, 7]), 7]
    return(Drawdown_Sim(input$age_sd[2], start_capital_sd, input$withdraw_freq_sd, input$annual_mean_return_sd, input$annual_ret_std_dev_sd, input$annual_inflation_sd, input$annual_inf_std_dev_sd, input$n_sim_sd, annual_withdrawals = input$annual_withdrawals_sd))
  }),
  
  output$drawdown_ruin_prob_sd <- renderText({
    Spaths <- drawdown_react_sd()
    p = p_list[match(input$withdraw_freq_sd, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$age_sd[2])
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / input$n_sim_sd
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$drawdown_average_fund_sd <- renderText({
    Spaths <- drawdown_react_sd()
    p = p_list[match(input$withdraw_freq_sd, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$age_sd[2])
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_sim_plot_sd <- renderPlot({
    Spaths <- drawdown_react_sd()
    dat <- vector("list", input$n_sim_sd)
    p <- ggplot()
    for (i in seq(input$n_sim_sd)) {
      dat[[i]] <- data.frame(time = (1:((p_list[match(input$withdraw_freq_sd, freq_list)] * exn(ILT15_female_reduced, input$age_sd[2])) +1)), capital = Spaths[i,])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
    } 
    return(p)
  }),
  
  output$life_ex_sd <- renderText({
    ex = exn(ILT15_female_reduced, input$age_sd[2])
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE), " Years"))
  }),
  
  observeEvent(input$resim_sd, {
    updateNumericInputIcon(session, "investCharge_sd", value = input$investCharge_sd + 1)
    updateNumericInputIcon(session, "investCharge_sd", value = input$investCharge_sd)
  })
)