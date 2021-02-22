list(
  drawdown_react_bh <- reactive({
    broken_heart_lifetable = broken_heart_life_table(input$widowed_status_bh, input$widowed_bh, input$gender_bh)
    return(Drawdown_Sim(input$age_bh, input$start_capital_bh, input$withdraw_freq_bh, input$annual_mean_return_bh, input$annual_ret_std_dev_bh, input$annual_inflation_bh, input$annual_inf_std_dev_bh, percent_yn = input$percent_yn_bh, annual_withdrawals = input$annual_withdrawals_bh, percent_withdrawal = input$percent_withdrawal_bh, life_table = broken_heart_lifetable))
  }),
  
  output$life_ex_bh <- renderText({
    broken_heart_lifetable = broken_heart_life_table(input$widowed_status_bh, input$widowed_bh, input$gender_bh)
    ex = exn(broken_heart_lifetable, input$age_bh)
    return(c(tommy_round(ex), " Years"))
  }),
  
  output$life_ex_no_bh <- renderText({
    broken_heart_lifetable = broken_heart_life_table(F, input$widowed_bh, input$gender_bh)
    ex = exn(broken_heart_lifetable, input$age_bh)
    return(c(tommy_round(ex), " Years"))
  }),
  
  output$life_ex_diff_bh <- renderText({
    broken_heart_lifetable_widow = broken_heart_life_table(T, input$widowed_bh, input$gender_bh)
    broken_heart_lifetable_no = broken_heart_life_table(F, input$widowed_bh, input$gender_bh)
    diff = exn(broken_heart_lifetable_no, input$age_bh) - exn(broken_heart_lifetable_widow, input$age_bh)
    return(c(tommy_round(diff), " Years"))
  }),
  
  output$drawdown_average_fund_bh <- renderText({
    Spaths <- drawdown_react_bh()
    broken_heart_lifetable = broken_heart_life_table(input$widowed_status_bh, input$widowed_bh, input$gender_bh)
    p = p_list[match(input$withdraw_freq_bh, freq_list)]
    n.obs =  p * exn(broken_heart_lifetable, input$age_bh)
    average = mean(Spaths[, n.obs])
    return(c("€", tommy_round(average)))
  }),
  
  output$drawdown_ruin_prob_bh <- renderText({
    Spaths <- drawdown_react_bh()
    broken_heart_lifetable = broken_heart_life_table(input$widowed_status_bh, input$widowed_bh, input$gender_bh)
    p = p_list[match(input$withdraw_freq_bh, freq_list)]
    n.obs =  p * exn(broken_heart_lifetable, input$age_bh)
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / 10000
    return(c(tommy_round(ruin), "%"))
  }),
  
  output$table_d_bh <- renderDataTable({
    Spaths <- drawdown_react_bh()
    broken_heart_lifetable = broken_heart_life_table(input$widowed_status_bh, input$widowed_bh, input$gender_bh)
    p = p_list[match(input$withdraw_freq_bh, freq_list)]
    average = as.numeric(p * (getOmega(broken_heart_lifetable) - input$age_bh))
    prob_ruin = as.numeric(p * (getOmega(broken_heart_lifetable) - input$age_bh))
    years = rep(1:(getOmega(broken_heart_lifetable) - input$age_bh), each = p)
    for(i in 1:(p * (getOmega(broken_heart_lifetable) - input$age_bh))){
      average[i] = mean(Spaths[, i])
      prob_ruin[i] = (length(which(Spaths[, i] == 0))) / 10000
    }
    important_years = seq(5, getOmega(broken_heart_lifetable) - input$age_bh, 5)
    points = p * important_years
    table_df = data.frame(years, average, prob_ruin)
    table_df = table_df[points, ]
    colnames(table_df) = c("Years", "Final Average Fund Value", "Probability of Ruin")
    table <- datatable(table_df, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    table <- formatCurrency(table, columns = "Final Average Fund Value", currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    return(table)
  }),
  
  output$life_ex_change_plot_bh <- renderPlot({
    broken_heart_lifetable_widow = broken_heart_life_table(T, input$widowed_bh, input$gender_bh)
    broken_heart_lifetable_no = broken_heart_life_table(F, input$widowed_bh, input$gender_bh)
    bh_w_qx = bh_n_qx = as.numeric(getOmega(broken_heart_lifetable_widow))
    for(i in 1:getOmega(broken_heart_lifetable_widow)){
      bh_w_qx[i] = (broken_heart_lifetable_widow@lx[i] - broken_heart_lifetable_widow@lx[i + 1])/broken_heart_lifetable_widow@lx[i]
      bh_n_qx[i] = (broken_heart_lifetable_no@lx[i] - broken_heart_lifetable_no@lx[i + 1])/broken_heart_lifetable_no@lx[i]
    }
    diff_qx = ((bh_w_qx - bh_n_qx) * 100)/bh_n_qx
    df = data.frame(age = input$widowed_bh:(input$widowed_bh + 4), diff_qx = diff_qx[(input$widowed_bh + 1):(input$widowed_bh + 5)])
    ggplot(df, aes(x = age, y = diff_qx, fill="#4A8DBF", color="#4A8DBF")) + xlab("Age") + ylab("% Difference in Prob of Death (qx)") +
      geom_bar(stat = "identity", color = "#4A8DBF", fill = "#4A8DBF")
  }),
  
  output$qx_change_plot_bh <- renderPlot({
    bh_w_f = broken_heart_life_table(T, input$widowed_bh, 1)
    bh_no_f = broken_heart_life_table(F, input$widowed_bh, 1)
    bh_w_m = broken_heart_life_table(T, input$widowed_bh, 2)
    bh_no_m = broken_heart_life_table(F, input$widowed_bh, 2)
    bh_w_f_qx = bh_no_f_qx = bh_w_m_qx = bh_no_m_qx = as.numeric(getOmega(bh_w_f))
    for(i in 1:getOmega(bh_w_f) + 1){
      bh_w_f_qx[i] = (bh_w_f@lx[i] - bh_w_f@lx[i + 1])/bh_w_f@lx[i]
      bh_no_f_qx[i] = (bh_no_f@lx[i] - bh_no_f@lx[i + 1])/bh_no_f@lx[i]
      bh_w_m_qx[i] = (bh_w_m@lx[i] - bh_w_m@lx[i + 1])/bh_w_m@lx[i]
      bh_no_m_qx[i] = (bh_no_m@lx[i] - bh_no_m@lx[i + 1])/bh_no_m@lx[i]
    }
    df = data.frame(age = (input$widowed_bh - 1):(input$widowed_bh + 4), 
                    female_widowed = bh_w_f_qx[(input$widowed_bh):(input$widowed_bh + 5)],
                    female_not = bh_no_f_qx[(input$widowed_bh):(input$widowed_bh + 5)],
                    male_widowed = bh_w_m_qx[(input$widowed_bh):(input$widowed_bh + 5)],
                    male_not = bh_no_m_qx[(input$widowed_bh):(input$widowed_bh + 5)]
                    )
    
    ggplot(df, aes(x = age)) + xlab("Age") + ylab("Difference in Prob of Death (qx)") +
      geom_line(aes(y = female_widowed, colour = 'Female', linetype = 'Widowed')) + geom_line(aes(y = female_not, colour = 'Female', linetype = 'Baseline')) + geom_line(aes(y = male_widowed, colour = "Male", linetype = 'Widowed')) + geom_line(aes(y = male_not, colour = "Male", linetype = "Baseline")) + 
      scale_color_manual("Gender", values = c('hotpink3', "#4A8DBF")) + scale_linetype_manual("Widowhood Status", values = c("Widowed" = 1, "Baseline" = 2)) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = 'top') + 
      guides(color = guide_legend(override.aes = list(size = 1), shape = guide_legend(override.aes = list(size = 6))))
  }),
  
  output$drawdown_sim_plot_bh <- renderPlot({
    Spaths <- drawdown_react_bh()
    broken_heart_lifetable = broken_heart_life_table(input$widowed_status_bh, input$widowed_bh, input$gender_bh)
    dat <- vector("list", input$n_sim_bh)
    p <- ggplot()
    for (i in seq(input$n_sim_bh)){
      dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq_bh, freq_list)] * (getOmega(broken_heart_lifetable) - input$age_bh)))), capital = Spaths[i,])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
    }
    return(p)
  }),
  
  observeEvent(input$resim_bh, {
    updateNumericInputIcon(session, "start_capital_bh", value = input$start_capital_bh + 1)
    updateNumericInputIcon(session, "start_capital_bh", value = input$start_capital_bh)
  }),
  
  observeEvent(input$widowed_status_bh,{
    if(input$widowed_status_bh == T) {
      enable("widowed_bh")
      updateNumericInputIcon(session, "widowed_bh", value = input$age_bh)
    } else {
      disable("widowed_bh")
      updateNumericInputIcon(session, "widowed_bh", value = NA)
    }
  }),
  
  observeEvent(input$percent_yn_bh,{
    if(input$percent_yn_bh == T) {
      updateNumericInputIcon(session, "percent_withdrawal_bh", value = 4)
      enable("percent_withdrawal_bh")
      disable("annual_withdrawals_bh")
      updateNumericInputIcon(session, "annual_withdrawals_bh", value = NA)
    } else {
      updateNumericInputIcon(session, "annual_withdrawals_bh", value = 15000)
      enable("annual_withdrawals_bh")
      disable("percent_withdrawal_bh")
      updateNumericInputIcon(session, "percent_withdrawal_bh", value = NA)
    }
  })
)