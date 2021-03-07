list(
  Seq_Sim <- function(seq_start_capital, seq_withdraw_freq, seq_annual_mean_return, seq_worst_annual_return, seq_best_annual_return, seq_annual_inflation, seq_n_years, seq_percent_yn = F, seq_annual_withdrawals, seq_percent_withdrawal = 4){
    
    #-------------------------------------
    #Assignment
    #-------------------------------------
    
    p = p_list[match(seq_withdraw_freq, freq_list)]
    start.capital = seq_start_capital
    
    # Investment
    annual.mean.return = seq_annual_mean_return / 100
    annual.worst.return = seq_worst_annual_return / 100
    annual.best.return = seq_best_annual_return / 100
    
    # Inflation
    annual.inflation = seq_annual_inflation / 100
    
    # Withdrawals
    if (seq_percent_yn == F) {
      periodic.withdrawals = seq_annual_withdrawals / p
    } else {
      periodic.percent.withdrawal = effective2Convertible(i=seq_percent_withdrawal/100,k=p)/p
    }
    
    # number of periods to simulate
    seq_n_obs = p * seq_n_years
    
    # periodic Investment and Inflation assumptions
    periodic.mean.return = effective2Convertible(i=annual.mean.return,k=p)/p
    periodic.worst.return = effective2Convertible(i=annual.worst.return,k=p)/p
    periodic.best.return = effective2Convertible(i=annual.best.return,k=p)/p
    periodic.inflation = effective2Convertible(i=annual.inflation,k=p)/p
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    Spaths = matrix(0, 3, seq_n_obs+1)
    periodic_withdrawals = matrix(0, 3, seq_n_obs)
    Spaths[,1] = start.capital
    
    periodic.invest.returns = matrix(0, 3, seq_n_obs)
    
    periodic.invest.returns[1,] = rep(seq(periodic.worst.return,periodic.best.return,length.out = seq_n_years), each=p)
    periodic.invest.returns[2,] = rep(periodic.mean.return,seq_n_obs)
    periodic.invest.returns[3,] = rep(seq(periodic.best.return,periodic.worst.return,length.out = seq_n_years), each=p)
    
    for(i in 1:3){
      for(j in 1:seq_n_obs){
        if (seq_percent_yn == F) {
          if (Spaths[i,j] <= 0) {
            break
          } 
          else if (Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)-periodic.withdrawals <= 0) {
            break
          } 
          else {
            Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)-periodic.withdrawals
            periodic_withdrawals[i, j] = periodic.withdrawals
          }
        } 
        else {
          if (Spaths[i,j] <= 0) {
            break
          } else if (Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)*(1 - (periodic.percent.withdrawal * (1+periodic.inflation)^j)) <= 0) {
            break
          } else {
            Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)*(1 - (periodic.percent.withdrawal * (1+periodic.inflation)^j))
            periodic_withdrawals[i, j] = Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)*(periodic.percent.withdrawal * (1+periodic.inflation)^j)
          }
        }
      }
    }
    return(Spaths)
  },
  
  sequencing_react <- reactive({
    
    validate(
      need(input$seq_start_capital != "" && input$seq_annual_mean_return != "" && input$seq_best_annual_return != "" &&
           input$seq_annual_inflation != "" && input$seq_n_years != "" && input$seq_annual_withdrawals != "", 
           "Please ensure that all inputs are provided")
    )
    validate(
      need(input$seq_best_annual_return > input$seq_annual_mean_return, 
           "Please ensure that the mean annual return is less than the highest annual return achieved")
    )
    
    return(Seq_Sim(input$seq_start_capital,
                   input$seq_withdraw_freq ,
                   input$seq_annual_mean_return,
                   input$seq_worst_annual_return,
                   input$seq_best_annual_return,
                   input$seq_annual_inflation,
                   input$seq_n_years,
                   seq_annual_withdrawals = input$seq_annual_withdrawals))
  }),
  
  
  output$sequencing_sim_plot_three <- renderPlotly({
    
    p = p_list[match(input$seq_withdraw_freq, freq_list)]
    # number of periods to simulate
    seq_n_obs = p * input$seq_n_years
    
    Spaths <- sequencing_react()
    
    df = data.frame(Time = rep(0:seq_n_obs,3),
                    Capital = c(Spaths[1,],Spaths[2,],Spaths[3,]),
                    Market = c(rep("Bad Early Years", seq_n_obs + 1), 
                               rep("Average Early Years", seq_n_obs + 1), 
                               rep("Good Early Years", seq_n_obs + 1)))
    
    graph = ggplot(data = df, aes(x = Time, y = Capital, color = Market)) +
      geom_line() +
      geom_point() +
      ggtitle("Drawdown") +
      theme_ipsum() +
      ylab("Capital") +
      theme(
        legend.position = c(.95, .80),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )
    
    return(ggplotly(graph))
  }),
  
  observeEvent(input$seq_best_annual_return,{
    req(input$seq_annual_mean_return,input$seq_best_annual_return)
    updateNumericInputIcon(session, "seq_worst_annual_return", value = 2*input$seq_annual_mean_return - input$seq_best_annual_return)
    disable("seq_worst_annual_return") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),
  
  observeEvent(input$seq_annual_mean_return,{
    req(input$seq_annual_mean_return,input$seq_best_annual_return)
    updateNumericInputIcon(session, "seq_worst_annual_return", value = input$seq_annual_mean_return - (input$seq_best_annual_return - input$seq_annual_mean_return))
  })
  
)