list(
  SORP_react_sd <- reactive({
    return(SORP(input$age_sd[1], input$age_sd[2], input$relationship_sd, input$sal_sd, input$fundvalue_sd, input$PreK_sd, input$PostK_sd, input$emp_contri_sd, input$empr_contri_sd, input$salEsc_sd, input$iPost_sd, input$annEsc_sd, input$guaranteed_sd, input$equity_sd, input$fixed_sd, input$cash_sd, input$investCharge_sd, input$equity_p_sd, input$fixed_p_sd, input$cash_p_sd))
  }),
  
  SORP_react_sd_s <- reactive({
    return(SORP(input$age_sd_s[1], input$age_sd_s[2], input$relationship_sd, input$sal_sd_s, input$fundvalue_sd_s, input$PreK_sd_s, input$PostK_sd_s, input$emp_contri_sd_s, input$empr_contri_sd_s, input$salEsc_sd, input$iPost_sd, input$annEsc_sd, input$guaranteed_sd, input$equity_sd, input$fixed_sd, input$cash_sd, input$investCharge_sd, input$equity_p_sd, input$fixed_p_sd, input$cash_p_sd))
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
    updateSliderInput(session, "equity_p_sd", value = 4.5)
    updateSliderInput(session, "fixed_p_sd", value = 1)
    updateSliderInput(session, "cash_p_sd", value = 0)
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
    sorp <- datatable(sorp, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    sorp <- formatCurrency(sorp, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Fund Value at end of Period"), currency = "€")
    return(sorp)
  }),
  
  output$plot_sd_s <- renderPlot({
    
    AgeandFundValue <- SORP_react_sd_s() %>% select(age_exact, FundValue)
    
    ggplot(AgeandFundValue, aes(x=age_exact, y=FundValue, fill="#4A8DBF", color="#4A8DBF")) +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(y="Fund Value", x = "Age", fill = NULL, color = NULL) +
      theme(legend.position = "none") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  }),
  
  output$fundFV_sd_s <- renderText({
    sorp <- SORP_react_sd_s()
    preK = p_list[match(input$PreK_sd_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    fund_FV <- fundvalue_at_retirement[(input$age_sd_s[2] - input$age_sd_s[1])*preK + 1, 1]
    return(c("€", format(round(as.numeric(fund_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentFV_sd_s <- renderText({
    sorp <- SORP_react_sd_s()
    preK = p_list[match(input$PreK_sd_s, freq_list)]
    postK = p_list[match(input$PostK_sd_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    periodic_payment_FV = (fundvalue_at_retirement[(input$age_sd_s[2] - input$age_sd_s[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_FV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$fundCV_sd_s <- renderText({
    sorp <- SORP_react_sd_s()
    preK = p_list[match(input$PreK_sd_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_sd/100)^(input$age_sd_s[2] - input$age_sd_s[1]))
    fund_CV = fundvalue_at_retirement[(input$age_sd_s[2] - input$age_sd_s[1])*preK + 1, 1] * discount_factor
    return(c("€", format(round(as.numeric(fund_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$pensionPaymentCV_sd_s <- renderText({
    sorp <- SORP_react_sd_s()
    preK = p_list[match(input$PreK_sd_s, freq_list)]
    postK = p_list[match(input$PostK_sd_s, freq_list)]
    fundvalue_at_retirement <- sorp %>% select(FundValue)
    discount_factor = 1/((1 + input$discountRate_sd/100)^(input$age_sd_s[2] - input$age_sd_s[1]))
    periodic_payment_CV = discount_factor * (fundvalue_at_retirement[(input$age_sd_s[2] - input$age_sd_s[1])*preK + 1, 1]/sorp[1, 8])/postK
    return(c("€", format(round(as.numeric(periodic_payment_CV), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$table_sd_s <- renderDataTable({
    sorp = SORP_react_sd_s()
    preK = p_list[match(input$PreK_sd_s, freq_list)]
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
  
  observeEvent(input$equity_sd,{
    updateSliderInput(session, "fixed_sd", max = 100 - input$equity_sd)
    disable("cash_sd") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),
  
  observe({
    updateSliderInput(session, "cash_sd", value = 100 - input$equity_sd - input$fixed_sd)
  }),
  
  observeEvent(input$age_sd[2],{
    updateNumericInputIcon(session, "guaranteed_sd", max = getOmega(ILT15_female_reduced) - input$age_sd[2])
  }),
  
  observeEvent(input$relationship_sd,{
    if(input$relationship_sd != 1) {
      shinyjs::show(id = "spouse_sorp_parameters")
      shinyjs::show(id = "spouse_sorp_summary")
    }
    if(input$relationship_sd == 1){
      shinyjs::hide(id = "spouse_sorp_parameters")
      shinyjs::hide(id = "spouse_sorp_summary")
    }
  }),
  
  drawdown_react_sd <- reactive({
    sorp <- SORP_react_sd()
    if(input$relationship_sd != 1){
      sorp_s <- SORP_react_sd_s()
      start_capital_sd = sorp[length(sorp[, 7]), 7] + sorp_s[length(sorp[, 7]), 7]
    } else {
      start_capital_sd = sorp[length(sorp[, 7]), 7]
    }
    return(Drawdown_Sim(input$age_sd[2], start_capital_sd, input$withdraw_freq_sd, input$annual_mean_return_sd, input$annual_ret_std_dev_sd, input$annual_inflation_sd, input$annual_inf_std_dev_sd, percent_yn = input$percent_yn_sd, annual_withdrawals = input$annual_withdrawals_sd, percent_withdrawal = input$percent_withdrawal_sd, retire_age_spouse = input$age_sd_s[2])[[1]])
  }),
  
  output$life_ex_sd <- renderText({
    if(input$relationship_sd != 1){
      ex = max(exn(ILT15_female_reduced, input$age_sd[2]), exn(ILT15_male_reduced, input$age_sd_s[2]))
    } else {
      ex = exn(ILT15_female_reduced, input$age_sd[2])
    }
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE), " Years"))
  }),
  
  output$drawdown_average_fund_sd <- renderText({
    Spaths <- drawdown_react_sd()
    p = p_list[match(input$withdraw_freq_sd, freq_list)]
    if(input$relationship_sd != 1){
      n.obs =  p * max(exn(ILT15_female_reduced, input$age_sd[2]), exn(ILT15_male_reduced, input$age_sd_s[2]))
    } else {
      n.obs =  p * exn(ILT15_female_reduced, input$age_sd[2])
    }
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_ruin_prob_sd <- renderText({
    Spaths <- drawdown_react_sd()
    p = p_list[match(input$withdraw_freq_sd, freq_list)]
    if(input$relationship_sd != 1){
      n.obs =  p * max(exn(ILT15_female_reduced, input$age_sd[2]), exn(ILT15_male_reduced, input$age_sd_s[2]))
    } else {
      n.obs =  p * exn(ILT15_female_reduced, input$age_sd[2])
    }
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / 10000
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$table_d_sd <- renderDataTable({
    Spaths <- drawdown_react_sd()
    p = p_list[match(input$withdraw_freq_sd, freq_list)]
    average = as.numeric(p * (getOmega(ILT15_female_reduced) - input$age_sd[2]))
    prob_ruin = as.numeric(p * (getOmega(ILT15_female_reduced) - input$age_sd[2]))
    years = rep(1:(getOmega(ILT15_female_reduced) - input$age_sd[2]), each = p)
    for(i in 1:(p * (getOmega(ILT15_female_reduced) - input$age_sd[2]))){
      average[i] = mean(Spaths[, i])
      prob_ruin[i] = (length(which(Spaths[, i] == 0))) / 10000
    }
    important_years = seq(5, getOmega(ILT15_female_reduced) - input$age_sd[2], 5)
    points = p * important_years
    table_df = data.frame(years, average, prob_ruin)
    table_df = table_df[points, ]
    colnames(table_df) = c("Years", "Final Average Fund Value", "Probability of Ruin")
    table <- datatable(table_df, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    table <- formatCurrency(table, columns = "Final Average Fund Value", currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    return(table)
  }),
  
  output$drawdown_sim_plot_sd <- renderPlot({
    Spaths <- drawdown_react_sd()
    dat <- vector("list", input$n_sim_sd)
    p <- ggplot()
    if(input$relationship_sd != 1){
      for (i in seq(input$n_sim_sd)){
        dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq_sd, freq_list)] * (getOmega(ILT15_female_reduced) - input$age_sd[2])))), capital = Spaths[i,])
        p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
      }
    } else {
      for (i in seq(input$n_sim_sd)){
        dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq_sd, freq_list)] * (getOmega(ILT15_female_reduced) - input$age_sd[2])))), capital = Spaths[i,])
        p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
      }
    }
    return(p)
  }),
  
  observeEvent(input$resim_sd, {
    updateNumericInputIcon(session, "investCharge_sd", value = input$investCharge_sd + 1)
    updateNumericInputIcon(session, "investCharge_sd", value = input$investCharge_sd)
  }),
  
  observeEvent(input$percent_yn_sd,{
    if(input$percent_yn_sd == T) {
      updateNumericInputIcon(session, "percent_withdrawal_sd", value = 4)
      enable("percent_withdrawal_sd")
      disable("annual_withdrawals_sd")
      updateNumericInputIcon(session, "annual_withdrawals_sd", value = NA)
    } else {
      updateNumericInputIcon(session, "annual_withdrawals_sd", value = 15000)
      enable("annual_withdrawals_sd")
      disable("percent_withdrawal_sd")
      updateNumericInputIcon(session, "percent_withdrawal_sd", value = NA)
    }
  })
)