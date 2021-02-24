list(
  sorp_annuity_pd <- reactive({
    return(SORP_Annuity(input$age_pd[1], relationship = input$relationship_pd, PostK = input$withdraw_freq_pd))
  }),

  sorp_annuity_buy_later_pd <- reactive({
    return(SORP_Annuity(input$age_pd[1], input$age_pd[2], input$relationship_pd, input$withdraw_freq_pd, guaranteed = 0))
  }),

  sorp_annuity_deferred_pd <- reactive({
    return(SORP_Annuity(input$age_pd[1], input$age_pd[2], input$relationship_pd, input$withdraw_freq_pd, guaranteed = 0, deferred = T))
  }),

  drawdown_react_pd <- reactive({
    return(Drawdown_Sim(input$age_pd[1], input$start_capital_pd, input$withdraw_freq_pd, input$annual_mean_return_pd, input$annual_ret_std_dev_pd, input$annual_inflation_pd, input$annual_inf_std_dev_pd, percent_yn = input$percent_yn_pd_d, annual_withdrawals = input$annual_withdrawals_pd_d, percent_withdrawal = input$percent_withdrawal_pd_d))
  }),

  drawdown_react_buy_later_pd <- reactive({
    return(Drawdown_Sim(input$age_pd[1], input$start_capital_pd, input$withdraw_freq_pd, input$annual_mean_return_pd, input$annual_ret_std_dev_pd, input$annual_inflation_pd, input$annual_inf_std_dev_pd, percent_yn = input$percent_yn_pd_bl, annual_withdrawals = input$annual_withdrawals_pd_bl, percent_withdrawal = input$percent_withdrawal_pd_bl, end_age = input$age_pd[2]))
  }),

  drawdown_deferred_pd <- reactive({
    sorp_annuity = sorp_annuity_pd()
    p = p_list[match(input$withdraw_freq_pd, freq_list)]
    periodic_payment = input$start_capital_pd / sorp_annuity / p
    
    deferred_annuity = sorp_annuity_deferred_pd()
    cost_deferred = deferred_annuity * periodic_payment * p
    new_start_capital = input$start_capital_pd - cost_deferred
    return(c(periodic_payment, deferred_annuity, cost_deferred, new_start_capital))
  }), 
  
  drawdown_react_deferred_pd <- reactive({
    drawdown_deferred_pd = drawdown_deferred_pd()
    new_start_capital = drawdown_deferred_pd[4]
    return(Drawdown_Sim(input$age_pd[1], new_start_capital, input$withdraw_freq_pd, input$annual_mean_return_pd, input$annual_ret_std_dev_pd, input$annual_inflation_pd, input$annual_inf_std_dev_pd, percent_yn = input$percent_yn_pd_da, annual_withdrawals = input$annual_withdrawals_pd_da, percent_withdrawal = input$percent_withdrawal_pd_da, end_age = input$age_pd[2]))
  }),

  output$life_ex_pd <- renderText({
    ex = exn(ILT15_female_reduced, input$age_pd[1])
    return(c(tommy_round(ex), " Years"))
  }),
  
  output$sorp_payment_pd <- renderText({
    sorp_annuity = sorp_annuity_pd()
    p = p_list[match(input$withdraw_freq_pd, freq_list)]
    payment = (input$start_capital_pd / sorp_annuity / p)
    return(c("€", tommy_round(payment)))
  }),
  
  average_fund_pd_bl <- reactive({
    Spaths <- drawdown_react_buy_later_pd()
    p = p_list[match(input$withdraw_freq_pd, freq_list)]
    n.obs =  p * (input$age_pd[2] - input$age_pd[1])
    return(mean(Spaths[, n.obs]))
  }),
  
  output$drawdown_average_fund_pd_bl <- renderText({
    average = average_fund_pd_bl()
    return(c("€", tommy_round(average)))
  }),

  output$average_annuity_pd_bl <- renderText({
    average = average_fund_pd_bl()
    sorp_annuity = sorp_annuity_buy_later_pd()
    p = p_list[match(input$withdraw_freq_pd, freq_list)]
    discount_factor = 1/((1 + 2.5/100)^(input$age_pd[2] - input$age_pd[1]))
    payment = discount_factor * (average / sorp_annuity / p)
    return(c("€", tommy_round(payment)))
  }),
  
  average_fund_pd_da <- reactive({
    Spaths <- drawdown_react_deferred_pd()
    p = p_list[match(input$withdraw_freq_pd, freq_list)]
    n.obs =  p * (input$age_pd[2] - input$age_pd[1])
    return(mean(Spaths[, n.obs]))
  }),
  
  output$drawdown_average_fund_pd_da <- renderText({
    average = average_fund_pd_da()
    return(c("€", tommy_round(average)))
  }),
  
  output$average_annuity_pd_da <- renderText({
    drawdown_deferred_pd = drawdown_deferred_pd()
    periodic_payment = drawdown_deferred_pd[1]
    return(c("€", tommy_round(periodic_payment)))
  }),
  
  output$cost_annuity_pd_da <- renderText({
    drawdown_deferred_pd = drawdown_deferred_pd()
    cost_annuity = drawdown_deferred_pd[3]
    return(c("€", tommy_round(cost_annuity)))
  }),

  output$table_d_pd <- renderDataTable({
    Spaths <- drawdown_react_pd()
    p = p_list[match(input$withdraw_freq_pd, freq_list)]
    average = as.numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    prob_ruin = as.numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    years = rep(1:((getOmega(ILT15_female_reduced) - input$age_pd[1])), each = p)
    for(i in 1:(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))){
      average[i] = mean(Spaths[, i])
      prob_ruin[i] = (length(which(Spaths[, i] == 0))) / 10000
    }
    important_years = seq(5, (getOmega(ILT15_female_reduced) - input$age_pd[1]), 5)
    points = p * important_years
    table_df = data.frame(years, average, prob_ruin)
    table_df = table_df[points, ]
    colnames(table_df) = c("Years", "Final Average Fund Value", "Probability of Ruin")
    table <- datatable(table_df, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    table <- formatCurrency(table, columns = "Final Average Fund Value", currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    return(table)
  }),

  observeEvent(input$resim_pd, {
    updateNumericInputIcon(session, "start_capital_pd", value = input$start_capital_pd + 1)
    updateNumericInputIcon(session, "start_capital_pd", value = input$start_capital_pd)
  }),

  observeEvent(input$percent_yn_pd_d,{
    if(input$percent_yn_pd_d == T) {
      updateNumericInputIcon(session, "percent_withdrawal_pd_d", value = 4)
      enable("percent_withdrawal_pd_d")
      disable("annual_withdrawals_pd_d")
      updateNumericInputIcon(session, "annual_withdrawals_pd_d", value = NA)
    } else {
      updateNumericInputIcon(session, "annual_withdrawals_pd_d", value = 15000)
      enable("annual_withdrawals_pd_d")
      disable("percent_withdrawal_pd_d")
      updateNumericInputIcon(session, "percent_withdrawal_pd_d", value = NA)
    }
  }),
  
  observeEvent(input$percent_yn_pd_bl,{
    if(input$percent_yn_pd_bl == T) {
      updateNumericInputIcon(session, "percent_withdrawal_pd_bl", value = 4)
      enable("percent_withdrawal_pd_bl")
      disable("annual_withdrawals_pd_bl")
      updateNumericInputIcon(session, "annual_withdrawals_pd_bl", value = NA)
    } else {
      updateNumericInputIcon(session, "annual_withdrawals_pd_bl", value = 15000)
      enable("annual_withdrawals_pd_bl")
      disable("percent_withdrawal_pd_bl")
      updateNumericInputIcon(session, "percent_withdrawal_pd_bl", value = NA)
    }
  }),
  
  observeEvent(input$percent_yn_pd_da,{
    if(input$percent_yn_pd_da == T) {
      updateNumericInputIcon(session, "percent_withdrawal_pd_da", value = 4)
      enable("percent_withdrawal_pd_da")
      disable("annual_withdrawals_pd_da")
      updateNumericInputIcon(session, "annual_withdrawals_pd_da", value = NA)
    } else {
      updateNumericInputIcon(session, "annual_withdrawals_pd_da", value = 15000)
      enable("annual_withdrawals_pd_da")
      disable("percent_withdrawal_pd_da")
      updateNumericInputIcon(session, "percent_withdrawal_pd_da", value = NA)
    }
  })
)