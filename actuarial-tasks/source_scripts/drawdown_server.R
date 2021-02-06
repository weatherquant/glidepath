list(
  Drawdown_Sim <- reactive({
    
    #-------------------------------------
    #Assignment
    #-------------------------------------
    
    p = p_list[match(input$withdraw_freq, freq_list)]
    start.capital = input$start_capital
    
    # Investment
    annual.mean.return = input$annual_mean_return / 100
    annual.ret.std.dev = input$annual_ret_std_dev / 100
    
    # Inflation
    annual.inflation = input$annual_inflation / 100
    annual.inf.std.dev = input$annual_inf_std_dev / 100
    
    # Withdrawals
    periodic.withdrawls = input$annual_withdrawals / p
    
    # Number of observations (in Years)
    n.years = input$n_years
    
    # Number of simulations
    n.sim = input$n_sim
    
    # number of periods to simulate
    n.obs = p * n.years
    
    # periodic Investment and Inflation assumptions
    periodic.mean.return = effective2Convertible(i=annual.mean.return,k=p)/p
    periodic.ret.std.dev = annual.ret.std.dev / sqrt(p)
    
    periodic.inflation = effective2Convertible(i=annual.inflation,k=p)/p
    periodic.inf.std.dev = annual.inf.std.dev / sqrt(p)
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    Spaths = matrix(0, n.sim, n.obs+1)
    Spaths[,1] = start.capital
    
    periodic.invest.returns = numeric(n.obs)
    periodic.inflation.returns = numeric(n.obs)
    
    for(i in 1:n.sim){
      
      periodic.invest.returns = rnorm(n.obs, mean = periodic.mean.return, sd = periodic.ret.std.dev)
      periodic.inflation.returns = rnorm(n.obs, mean = periodic.inflation, sd = periodic.inf.std.dev)
      if (Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawls <= 0) {break}
      Spaths[i,2] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawls
      
      for(j in 2:n.obs){
        if (Spaths[i,j] <= 0) {
          break
        } else if (Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawls <= 0) {
          break
        } else {
          Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawls
        }
      }
    }
    
    return(Spaths)
  }),
  
  output$drawdown_ruin_prob <- renderText({
    Spaths <- Drawdown_Sim()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * input$n_years
    ruin = (length(which(Spaths[, n.obs] == 0)) * 100) / input$n_sim
    return(c(format(round(as.numeric(ruin), 2), nsmall = 2, big.mark = ",", scientific=FALSE), "%"))
  }),
  
  output$drawdown_average_fund <- renderText({
    Spaths <- Drawdown_Sim()
    p = p_list[match(input$withdraw_freq, freq_list)]
    n.obs =  p * input$n_years
    average = mean(Spaths[, n.obs])
    return(c("€", format(round(as.numeric(average), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$drawdown_sim_plot <- renderPlot({
    Spaths <- Drawdown_Sim()
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