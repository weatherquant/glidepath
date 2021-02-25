list(
  SORP_react_bh <- reactive({
    return(SORP(input$age_bh[1], input$age_bh[2], input$relationship_bh, input$sal_bh, input$fundvalue_bh, input$PreK_bh, input$PostK_bh, input$emp_contri_bh, input$empr_contri_bh, input$salEsc_bh, input$iPost_bh, input$annEsc_bh, input$guaranteed_bh, input$equity_bh, input$fixed_bh, input$cash_bh, input$investCharge_bh, input$equity_p_bh, input$fixed_p_bh, input$cash_p_bh))
  }),
  
  SORP_react_bh_s <- reactive({
    return(SORP(input$age_bh_s[1], input$age_bh_s[2], input$relationship_bh, input$sal_bh_s, input$fundvalue_bh_s, input$PreK_bh_s, input$PostK_bh_s, input$emp_contri_bh_s, input$empr_contri_bh_s, input$salEsc_bh, input$iPost_bh, input$annEsc_bh, input$guaranteed_bh, input$equity_bh, input$fixed_bh, input$cash_bh, input$investCharge_bh, input$equity_p_bh, input$fixed_p_bh, input$cash_p_bh))
  }),
  
  observeEvent(input$default_bh, {
    updateNumericInputIcon(session, "salEsc_bh", value = 1.5)
    updateNumericInputIcon(session, "discountRate_bh", value = 2.5)
    updateNumericInputIcon(session, "iPost_bh", value = 0.5)
    updateNumericInputIcon(session, "annEsc_bh", value = 1)
    updateNumericInputIcon(session, "guaranteed_bh", value = 5)
    updateSliderInput(session, "equity_bh", value = 40)
    updateSliderInput(session, "fixed_bh", value = 30)
    updateSliderInput(session, "cash_bh", value = 30)
    updateNumericInputIcon(session, "investCharge_bh", value = 0.5)
    updateSliderInput(session, "equity_p_bh", value = 4.5)
    updateSliderInput(session, "fixed_p_bh", value = 1)
    updateSliderInput(session, "cash_p_bh", value = 0)
  }),
  
  output$plot_bh <- renderPlot({
    
    AgeandFundValue <- SORP_react_bh() %>% select(age_exact, FundValue)
    
    ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }),
  
  output$fundFV_bh <- renderText({
    sorp <- SORP_react_bh()
    preK = p_list[match(input$PreK_bh, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    fund_FV <- fundvalue_at_retirement[(input$age_bh[2] - input$age_bh[1])*preK + 1, 1]
    return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentFV_bh <- renderText({
    sorp <- SORP_react_bh()
    preK = p_list[match(input$PreK_bh, freq_list)]
    postK = p_list[match(input$PostK_bh, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    periodic_payment_FV = (fundvalue_at_retirement[(input$age_bh[2] - input$age_bh[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$fundCV_bh <- renderText({
    sorp <- SORP_react_bh()
    preK = p_list[match(input$PreK_bh, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_bh/100)^(input$age_bh[2] - input$age_bh[1]))
    fund_CV = fundvalue_at_retirement[(input$age_bh[2] - input$age_bh[1])*preK + 1, 1] * discount_factor
    return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentCV_bh <- renderText({
    sorp <- SORP_react_bh()
    preK = p_list[match(input$PreK_bh, freq_list)]
    postK = p_list[match(input$PostK_bh, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_bh/100)^(input$age_bh[2] - input$age_bh[1]))
    periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age_bh[2] - input$age_bh[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$table_bh <- renderDataTable({
    sorp = SORP_react_bh()
    preK = p_list[match(input$PreK_bh, freq_list)]
    if(preK == 1){
      sorp = data.frame(select(sorp, -period, -age_exact, -SORPAnnuity))
      colnames(sorp) = c("Age", "Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period")
    }else{
      sorp = data.frame(select(sorp, -age_exact, -SORPAnnuity))
      colnames(sorp) = c("Age", "Period", "Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period")
    }
    sorp <- datatable(sorp, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    sorp <- formatCurrency(sorp, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period"), currency = "€")
    return(sorp)
  }),
  
  output$plot_bh_s <- renderPlot({
    
    AgeandFundValue <- SORP_react_bh_s() %>% select(age_exact, FundValue)
    
    ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }),
  
  output$fundFV_bh_s <- renderText({
    sorp <- SORP_react_bh_s()
    preK = p_list[match(input$PreK_bh_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    fund_FV <- fundvalue_at_retirement[(input$age_bh_s[2] - input$age_bh_s[1])*preK + 1, 1]
    return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentFV_bh_s <- renderText({
    sorp <- SORP_react_bh_s()
    preK = p_list[match(input$PreK_bh_s, freq_list)]
    postK = p_list[match(input$PostK_bh_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    periodic_payment_FV = (fundvalue_at_retirement[(input$age_bh_s[2] - input$age_bh_s[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$fundCV_bh_s <- renderText({
    sorp <- SORP_react_bh_s()
    preK = p_list[match(input$PreK_bh_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_bh/100)^(input$age_bh_s[2] - input$age_bh_s[1]))
    fund_CV = fundvalue_at_retirement[(input$age_bh_s[2] - input$age_bh_s[1])*preK + 1, 1] * discount_factor
    return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentCV_bh_s <- renderText({
    sorp <- SORP_react_bh_s()
    preK = p_list[match(input$PreK_bh_s, freq_list)]
    postK = p_list[match(input$PostK_bh_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_bh/100)^(input$age_bh_s[2] - input$age_bh_s[1]))
    periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age_bh_s[2] - input$age_bh_s[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$table_bh_s <- renderDataTable({
    sorp = SORP_react_bh_s()
    preK = p_list[match(input$PreK_bh_s, freq_list)]
    if(preK == 1){
      sorp = data.frame(select(sorp, -period, -age_exact, -SORPAnnuity))
      colnames(sorp) = c("Age", "Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period")
    }else{
      sorp = data.frame(select(sorp, -age_exact, -SORPAnnuity))
      colnames(sorp) = c("Age", "Period", "Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period")
    }
    sorp <- datatable(sorp, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    sorp <- formatCurrency(sorp, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period"), currency = "€")
    return(sorp)
  }),
  
  observeEvent(input$equity_bh,{
    updateSliderInput(session, "fixed_bh", max = 100 - input$equity_bh)
    disable("cash_bh") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),
  
  observe({
    updateSliderInput(session, "cash_bh", value = 100 - input$equity_bh - input$fixed_bh)
  }),
  
  observeEvent(input$age_bh[2],{
    updateNumericInputIcon(session, "guaranteed_bh", max = getOmega(ILT15_female_reduced) - input$age_bh[2])
  }),
  
  observeEvent(input$relationship_bh,{
    if(input$relationship_bh != 1) {
      shinyjs::show(id = "spouse_sorp_parameters")
      shinyjs::show(id = "spouse_sorp_summary")
    }
    if(input$relationship_bh == 1){
      shinyjs::hide(id = "spouse_sorp_parameters")
      shinyjs::hide(id = "spouse_sorp_summary")
    }
    if(input$relationship_bh != 3) {
      disable("widowed_bh") 
      updateNumericInputIcon(session, "widowed_bh", value = NA)
    }
    if(input$relationship_bh == 3){
      enable("widowed_bh")
      updateNumericInputIcon(session, "widowed_bh", value = 70)
    }
  }),
  
  drawdown_react_bh <- reactive({
    sorp <- SORP_react_bh()
    if(input$relationship_bh != 1){
      sorp_s <- SORP_react_bh_s()
      start_capital_bh = sorp[length(sorp[, 7]), 7] + sorp_s[length(sorp[, 7]), 7]
    } else {
      start_capital_bh = sorp[length(sorp[, 7]), 7]
    }
    return(Drawdown_Sim(input$age_bh[2], start_capital_bh, input$withdraw_freq_bh, input$annual_mean_return_bh, input$annual_ret_std_dev_bh, input$annual_inflation_bh, input$annual_inf_std_dev_bh, percent_yn = input$percent_yn_bh, annual_withdrawals = input$annual_withdrawals_bh, percent_withdrawal = input$percent_withdrawal_bh, retire_age_spouse = input$age_bh_s[2])[[1]])
  }),
  
  output$life_ex_bh <- renderText({
    ILT15_female_reduced = ILT15_female_reduced_widowed(input$relationship_bh, input$widowed_bh)
    if(input$relationship_bh != 1){
      ex = max(exn(ILT15_female_reduced, input$age_bh[2]), exn(ILT15_male_reduced, input$age_bh_s[2]))
    } else {
      ex = exn(ILT15_female_reduced, input$age_bh[2])
    }
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE), " Years"))
  }),
  
  output$drawdown_average_fund_bh <- renderText({
    Spaths <- drawdown_react_bh()
    ILT15_female_reduced = ILT15_female_reduced_widowed(input$relationship_bh, input$widowed_bh)
    p = p_list[match(input$withdraw_freq_bh, freq_list)]
    if(input$relationship_bh != 1){
      n.obs =  p * max(exn(ILT15_female_reduced, input$age_bh[2]), exn(ILT15_male_reduced, input$age_bh_s[2]))
    } else {
      n.obs =  p * exn(ILT15_female_reduced, input$age_bh[2])
    }
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_ruin_prob_bh <- renderText({
    ILT15_female_reduced = ILT15_female_reduced_widowed(input$relationship_bh, input$widowed_bh)
    Spaths <- drawdown_react_bh()
    p = p_list[match(input$withdraw_freq_bh, freq_list)]
    if(input$relationship_bh != 1){
      n.obs =  p * max(exn(ILT15_female_reduced, input$age_bh[2]), exn(ILT15_male_reduced, input$age_bh_s[2]))
    } else {
      n.obs =  p * exn(ILT15_female_reduced, input$age_bh[2])
    }
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / 10000
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$table_d_bh <- renderDataTable({
    Spaths <- drawdown_react_bh()
    p = p_list[match(input$withdraw_freq_bh, freq_list)]
    average = as.numeric(p * (getOmega(ILT15_female_reduced) - input$age_bh[2]))
    prob_ruin = as.numeric(p * (getOmega(ILT15_female_reduced) - input$age_bh[2]))
    years = rep(1:(getOmega(ILT15_female_reduced) - input$age_bh[2]), each = p)
    for(i in 1:(p * (getOmega(ILT15_female_reduced) - input$age_bh[2]))){
      average[i] = mean(Spaths[, i])
      prob_ruin[i] = (length(which(Spaths[, i] == 0))) / 10000
    }
    important_years = seq(5, getOmega(ILT15_female_reduced) - input$age_bh[2], 5)
    points = p * important_years
    table_df = data.frame(years, average, prob_ruin)
    table_df = table_df[points, ]
    colnames(table_df) = c("Years", "Final Average Fund Value", "Probability of Ruin")
    table <- datatable(table_df, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    table <- formatCurrency(table, columns = "Final Average Fund Value", currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    return(table)
  }),
  
  output$drawdown_sim_plot_bh <- renderPlot({
    Spaths <- drawdown_react_bh()
    ILT15_female_reduced = ILT15_female_reduced_widowed(input$relationship_bh, input$widowed_bh)
    dat <- vector("list", input$n_sim_bh)
    p <- ggplot()
    if(input$relationship_bh != 1){
      for (i in seq(input$n_sim_bh)){
        dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq_bh, freq_list)] * (getOmega(ILT15_female_reduced) - input$age_bh[2])))), capital = Spaths[i,])
        p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
      }
    } else {
      for (i in seq(input$n_sim_bh)){
        dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq_bh, freq_list)] * (getOmega(ILT15_female_reduced) - input$age_bh[2])))), capital = Spaths[i,])
        p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
      }
    }
    return(p)
  }),
  
  observeEvent(input$resim_bh, {
    updateNumericInputIcon(session, "investCharge_bh", value = input$investCharge_bh + 1)
    updateNumericInputIcon(session, "investCharge_bh", value = input$investCharge_bh)
  }),
  
  observeEvent(input$percent_yn_bh,{
    if(input$percent_yn_bh == T) {
      # shinyjs::hide(id = "prob_ruin_box_bh")
      updateNumericInputIcon(session, "percent_withdrawal_bh", value = 4)
      enable("percent_withdrawal_bh")
      disable("annual_withdrawals_bh")
      updateNumericInputIcon(session, "annual_withdrawals_bh", value = NA)
    } else {
      # shinyjs::show(id = "prob_ruin_box_bh")
      updateNumericInputIcon(session, "annual_withdrawals_bh", value = 15000)
      enable("annual_withdrawals_bh")
      disable("percent_withdrawal_bh")
      updateNumericInputIcon(session, "percent_withdrawal_bh", value = NA)
    }
  })
)