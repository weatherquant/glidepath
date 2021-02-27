list(
  sorp_annuity_pd <- reactive({
    return(SORP_Annuity(input$age_pd[1], relationship = input$relationship_pd, PostK = "Annually"))
  }),

  sorp_annuity_buy_later_pd <- reactive({
    return(SORP_Annuity(input$age_pd[1], input$age_pd[2], input$relationship_pd, "Annually", guaranteed = 0))
  }),

  sorp_annuity_deferred_pd <- reactive({
    return(SORP_Annuity(input$age_pd[1], input$age_pd[2], input$relationship_pd, "Annually", guaranteed = 0, deferred = T))
  }),

  drawdown_react_pd <- reactive({
    return(Drawdown_Sim(input$age_pd[1], input$start_capital_pd, "Annually", input$annual_mean_return_pd, input$annual_ret_std_dev_pd, input$annual_inflation_pd, input$annual_inf_std_dev_pd, percent_yn = input$percent_yn_pd_d, annual_withdrawals = input$annual_withdrawals_pd_d, percent_withdrawal = input$percent_withdrawal_pd_d))
  }),

  drawdown_react_buy_later_pd <- reactive({
    return(Drawdown_Sim(input$age_pd[1], input$start_capital_pd, "Annually", input$annual_mean_return_pd, input$annual_ret_std_dev_pd, input$annual_inflation_pd, input$annual_inf_std_dev_pd, percent_yn = input$percent_yn_pd_bl, annual_withdrawals = input$annual_withdrawals_pd_bl, percent_withdrawal = input$percent_withdrawal_pd_bl, end_age = input$age_pd[2]))
  }),

  drawdown_deferred_pd <- reactive({
    sorp_annuity = sorp_annuity_pd()
    p = p_list[match("Annually", freq_list)]
    periodic_payment = input$start_capital_pd / sorp_annuity / p
    
    deferred_annuity = sorp_annuity_deferred_pd()
    cost_deferred = deferred_annuity * periodic_payment * p
    new_start_capital = input$start_capital_pd - cost_deferred
    return(c(periodic_payment, deferred_annuity, cost_deferred, new_start_capital))
  }), 
  
  drawdown_react_deferred_pd <- reactive({
    drawdown_deferred_pd = drawdown_deferred_pd()
    new_start_capital = drawdown_deferred_pd[4]
    return(Drawdown_Sim(input$age_pd[1], new_start_capital, "Annually", input$annual_mean_return_pd, input$annual_ret_std_dev_pd, input$annual_inflation_pd, input$annual_inf_std_dev_pd, percent_yn = input$percent_yn_pd_da, annual_withdrawals = input$annual_withdrawals_pd_da, percent_withdrawal = input$percent_withdrawal_pd_da, end_age = input$age_pd[2]))
  }),

  output$life_ex_pd <- renderText({
    ex = exn(ILT15_female_reduced, input$age_pd[1])
    return(c(tommy_round(ex), " Years"))
  }),
  
  output$sorp_payment_pd <- renderText({
    sorp_annuity = sorp_annuity_pd()
    p = p_list[match("Annually", freq_list)]
    payment = (input$start_capital_pd / sorp_annuity / p)
    return(c("€", tommy_round(payment)))
  }),
  
  average_fund_pd_d <- reactive({
    Spaths <- drawdown_react_pd()[[1]]
    p = p_list[match("Annually", freq_list)]
    n.obs =  p * (input$age_pd[2] - input$age_pd[1])
    return(mean(Spaths[, n.obs]))
  }),
  
  average_fund_pd_bl <- reactive({
    Spaths <- drawdown_react_buy_later_pd()[[1]]
    p = p_list[match("Annually", freq_list)]
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
    p = p_list[match("Annually", freq_list)]
    discount_factor = 1/((1 + 2.5/100)^(input$age_pd[2] - input$age_pd[1]))
    payment = discount_factor * (average / sorp_annuity / p)
    return(c("€", tommy_round(payment)))
  }),
  
  average_fund_pd_da <- reactive({
    Spaths <- drawdown_react_deferred_pd()[[1]]
    p = p_list[match("Annually", freq_list)]
    n.obs =  p * (input$age_pd[2] - input$age_pd[1])
    return(mean(Spaths[, n.obs]))
  }),
  
  output$drawdown_average_fund_pd_da <- renderText({
    average = average_fund_pd_da()
    return(c("€", tommy_round(average)))
  }),
  
  output$drawdown_average_fund_pd <- renderText({
    average = average_fund_pd_d()
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
  
  dataframe_outputs_pd <- reactive({
    p = p_list[match("Annually", freq_list)]
    
    drawdown <- drawdown_react_pd()
    drawdown_bl <- drawdown_react_buy_later_pd()
    drawdown_deferred <- drawdown_deferred_pd()
    drawdown_da <- drawdown_react_deferred_pd()
    
    discount_factor = 1/((1 + 2.5/100)^(input$age_pd[2] - input$age_pd[1]))
    
    sorp_annuity = sorp_annuity_pd()
    sorp_periodic_payment = input$start_capital_pd / sorp_annuity / p
    
    sorp_annuity_buy_later = sorp_annuity_buy_later_pd()
    average_fund_buy_later = average_fund_pd_bl()
    bl_periodic_payment = discount_factor * (average_fund_buy_later / sorp_annuity_buy_later / p)
    
    da_periodic_payment = drawdown_deferred[1]
    
    sorp_total_paid = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    drawdown_total_withdrawn = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    drawdown_average_fund = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    drawdown_prob_ruin = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    buy_later_total_withdrawn = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    buy_later_average_fund = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    buy_later_prob_ruin = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    buy_later_total_paid_annuity = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    deferred_total_withdrawn = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    deferred_average_fund = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    deferred_prob_ruin = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    deferred_total_paid_annuity = numeric(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))
    years = rep(1:((getOmega(ILT15_female_reduced) - input$age_pd[1])), each = p)
    
    sorp_total_paid[1] = sorp_periodic_payment
    drawdown_total_withdrawn[1] = mean(drawdown[[2]][, 1])
    drawdown_average_fund[1] = mean(drawdown[[1]][, 1])
    drawdown_prob_ruin[1] = (length(which(drawdown[[1]][, 1] == 0))) / 10000
    buy_later_total_withdrawn[1] = mean(drawdown_bl[[2]][, 1])
    buy_later_average_fund[1] = mean(drawdown_bl[[1]][, 1])
    buy_later_prob_ruin[1] = (length(which(drawdown_bl[[1]][, 1] == 0))) / 10000
    deferred_total_withdrawn[1] = mean(drawdown_da[[2]][, 1])
    deferred_average_fund[1] = mean(drawdown_da[[1]][, 1])
    deferred_prob_ruin[1] = (length(which(drawdown_da[[1]][, 1] == 0))) / 10000
    
    for(i in 2:(p * ((getOmega(ILT15_female_reduced) - input$age_pd[1])))){
      sorp_total_paid[i] = i * sorp_periodic_payment
      drawdown_total_withdrawn[i] = mean(rowSums(drawdown[[2]][, 1:i]))
      drawdown_average_fund[i] = mean(drawdown[[1]][, i])
      drawdown_prob_ruin[i] = (length(which(drawdown[[1]][, i] == 0))) * 100 / 10000
      
      if(i <= (p * (input$age_pd[2] - input$age_pd[1]))){
        buy_later_total_withdrawn[i] = mean(rowSums(drawdown_bl[[2]][, 1:i]))
        buy_later_average_fund[i] = mean(drawdown_bl[[1]][, i])
        buy_later_prob_ruin[i] = (length(which(drawdown_bl[[1]][, i] == 0))) * 100 / 10000
        deferred_total_withdrawn[i] = mean(rowSums(drawdown_da[[2]][, 1:i]))
        deferred_average_fund[i] = mean(drawdown_da[[1]][, i])
        deferred_prob_ruin[i] = (length(which(drawdown_da[[1]][, i] == 0))) * 100 / 10000
      } else if (i == (p * (input$age_pd[2] - input$age_pd[1])) + 1){
        buy_later_total_paid_annuity[i] = (i - (p * (input$age_pd[2] - input$age_pd[1]))) * bl_periodic_payment
        buy_later_total_withdrawn[i] = buy_later_total_withdrawn[i - 1]
        deferred_total_paid_annuity[i] = (i - (p * (input$age_pd[2] - input$age_pd[1]))) * da_periodic_payment
        deferred_total_withdrawn[i] = deferred_total_withdrawn[i - 1] + deferred_average_fund[i - 1]
      } else {
        buy_later_total_paid_annuity[i] = (i - (p * (input$age_pd[2] - input$age_pd[1]))) * bl_periodic_payment
        buy_later_total_withdrawn[i] = buy_later_total_withdrawn[i - 1]
        deferred_total_paid_annuity[i] = (i - (p * (input$age_pd[2] - input$age_pd[1]))) * da_periodic_payment
        deferred_total_withdrawn[i] = deferred_total_withdrawn[i - 1]
      }
    }
    buy_later_total = buy_later_total_paid_annuity + buy_later_total_withdrawn
    deferred_total = deferred_total_paid_annuity + deferred_total_withdrawn
    
    table_df = data.frame(years, sorp_total_paid, 
                          drawdown_total_withdrawn, drawdown_average_fund, drawdown_prob_ruin,
                          buy_later_total, buy_later_average_fund, 
                          deferred_total, deferred_average_fund, deferred_prob_ruin)
    
    return(table_df)
  }),
  
  output$compare_table_d_pd <- renderDataTable({
    table_df = dataframe_outputs_pd()
    p = p_list[match("Annually", freq_list)]
    
    table_df$sorp_total_paid = paste0("€", tommy_round(table_df$sorp_total_paid))
    table_df$drawdown_total_withdrawn = paste0("€", tommy_round(table_df$drawdown_total_withdrawn))
    table_df$buy_later_total = paste0("€", tommy_round(table_df$buy_later_total))
    table_df$deferred_total = paste0("€", tommy_round(table_df$deferred_total))
    table_df$drawdown_average_fund = paste0("€", tommy_round(table_df$drawdown_average_fund))
    table_df$buy_later_average_fund = paste0("€", tommy_round(table_df$buy_later_average_fund))
    table_df$deferred_average_fund = paste0("€", tommy_round(table_df$deferred_average_fund))
    table_df$drawdown_prob_ruin = paste0(tommy_round(table_df$drawdown_prob_ruin), "%")
    table_df$deferred_prob_ruin = paste0(tommy_round(table_df$deferred_prob_ruin), "%")
    
    important_years = seq(5, (getOmega(ILT15_female_reduced) - input$age_pd[1]), 5)
    points = p * important_years
    ex = round(p * exn(ILT15_female_reduced, input$age_pd[1]))
    points = sort(unique(c(points, ex)))

    table_df = table_df[points, ]
    
    container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Year', style="text-align:center; border-right: solid 1px"),
          th(colspan = 1, '100 % Annuity', style="text-align:center; border-right: solid 1px"),
          th(colspan = 3, '100% Drawdown', style="text-align:center; border-right: solid 1px"),
          th(colspan = 2, 'Drawdown and subsequent Annuity Purchase', style="text-align:center; border-right: solid 1px"),
          th(colspan = 3, 'Drawdown and Deferred Annuity', style="text-align:center")
        ),
        tr(
          th('Total Received', style = "border-right: solid 1px;"), 
          th('Total Withdrawn'), th('Average Fund'), th('Prob of Ruin', style = "border-right: solid 1px;"), 
          th('Total Withdrawn / Received'), th('Average Fund', style = "border-right: solid 1px;"), 
          th('Total Withdrawn / Received'), th('Average Fund'), th('Prob of Ruin')
        )
      )
    ))
    
    table <- datatable(table_df, container = container, options = list(paging = FALSE, searching = FALSE), rownames= FALSE) %>% 
      formatStyle(c(1, 2, 5, 7), `border-right` = "solid 1px") %>%
      formatStyle(
        'years',
        target = 'row',
        backgroundColor = styleEqual(ex, 'yellow')
      )
    return(table)
  }),
  
  output$income_compare <- renderPlotly({
    table_df = dataframe_outputs_pd()
    p = p_list[match("Annually", freq_list)]

    fig <- plot_ly(table_df, x = ~table_df$years, y = ~table_df$sorp_total_paid, name = "100 % Annuity", type = "scatter", mode = "lines", line = list(color = 'blue', width = 4))
    fig <- fig %>% add_trace(y = ~table_df$drawdown_total_withdrawn, name = '100% Drawdown',line = list(color = 'red', width = 4))
    fig <- fig %>% add_trace(y = ~table_df$buy_later_total, name = 'Drawdown and subsequent Annuity Purchase',line = list(color = 'green', width = 4))
    fig <- fig %>% add_trace(y = ~table_df$deferred_total, name = 'Drawdown and Deferred Annuity',line = list(color = 'orange', width = 4))
    fig <- fig %>% layout(xaxis = list(title = "Years since Retirement"), yaxis = list(title = "Amount Received €", range = c(0, round_any(max(table_df$sorp_total_paid, table_df$drawdown_total_withdrawn, table_df$buy_later_total, table_df$deferred_total),100000,f=ceiling))))
    fig <- fig %>% layout(legend = list(orientation = "h", y=1.2))
    fig <- fig %>%
      layout(hovermode = "x unified")
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