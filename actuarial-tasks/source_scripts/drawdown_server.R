list(
  drawdown_react <- reactive({
    return(Drawdown_Sim(input$start_capital, input$annual_withdrawals, input$withdraw_freq, input$annual_mean_return, input$annual_ret_std_dev, input$annual_inflation, input$annual_inf_std_dev, input$n_sim, input$n_years))
  }),
  
  output$drawdown_ruin_prob <- renderText({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * input$n_years
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / input$n_sim
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$drawdown_average_fund <- renderText({
    Spaths <- drawdown_react()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * input$n_years
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_sim_plot <- renderPlot({
    Spaths <- drawdown_react()
    dat <- vector("list", input$n_sim)
    p <- ggplot()
    for (i in seq(input$n_sim)) {
      dat[[i]] <- data.frame(time = (1:((p_list[match(input$withdraw_freq, freq_list)] * input$n_years) +1)), capital = Spaths[i,])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
    } 
    return(p)
  }),
  
  observeEvent(input$resim, {
    updateNumericInputIcon(session, "start_capital", value = input$start_capital + 1)
    updateNumericInputIcon(session, "start_capital", value = input$start_capital)
  })
)