list(
  drawdown_react <- reactive({
    return(Drawdown_Sim(input$retire_age, input$start_capital, input$withdraw_freq, input$annual_mean_return, input$annual_ret_std_dev, input$annual_inflation, input$annual_inf_std_dev, percent_yn = input$percent_yn, annual_withdrawals = input$annual_withdrawals, percent_withdrawal = input$percent_withdrawal)[[1]])
  }),
  
  output$life_ex <- renderText({
    ex = exn(ILT15_female_reduced, input$retire_age)
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE), " Years"))
  }),
  
  output$drawdown_average_fund <- renderText({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$retire_age)
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_ruin_prob <- renderText({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * exn(ILT15_female_reduced, input$retire_age)
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / 10000
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$table_d <- renderDataTable({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    average = as.numeric(p * (getOmega(ILT15_female_reduced) - input$retire_age))
    prob_ruin = as.numeric(p * (getOmega(ILT15_female_reduced) - input$retire_age))
    years = rep(1:(getOmega(ILT15_female_reduced) - input$retire_age), each = p)
    for(i in 1:(p * (getOmega(ILT15_female_reduced) - input$retire_age))){
      average[i] = mean(Spaths[, i])
      prob_ruin[i] = (length(which(Spaths[, i] == 0))) / 10000
    }
    important_years = seq(5, getOmega(ILT15_female_reduced) - input$retire_age, 5)
    points = p * important_years
    table_df = data.frame(years, average, prob_ruin)
    table_df = table_df[points, ]
    colnames(table_df) = c("Years", "Final Average Fund Value", "Probability of Ruin")
    table <- datatable(table_df, options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
    table <- formatCurrency(table, columns = "Final Average Fund Value", currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    return(table)
  }),
  
  output$drawdown_sim_plot <- renderPlot({
    Spaths <- drawdown_react()
    dat <- vector("list", input$n_sim)
    p <- ggplot()
    for (i in seq(input$n_sim)){
        dat[[i]] <- data.frame(time = (0:((p_list[match(input$withdraw_freq, freq_list)] * (getOmega(ILT15_female_reduced) - input$retire_age)))), capital = Spaths[i,])
        p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
      }
    return(p)
  }),
  
  observeEvent(input$resim, {
    updateNumericInputIcon(session, "start_capital", value = input$start_capital + 1)
    updateNumericInputIcon(session, "start_capital", value = input$start_capital)
  }),
  
  observeEvent(input$percent_yn,{
    if(input$percent_yn == T) {
      updateNumericInputIcon(session, "percent_withdrawal", value = 4)
      enable("percent_withdrawal")
      disable("annual_withdrawals")
      updateNumericInputIcon(session, "annual_withdrawals", value = NA)
    } else {
      updateNumericInputIcon(session, "annual_withdrawals", value = 15000)
      enable("annual_withdrawals")
      disable("percent_withdrawal")
      updateNumericInputIcon(session, "percent_withdrawal", value = NA)
    }
  })
)