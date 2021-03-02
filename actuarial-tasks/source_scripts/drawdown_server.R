list(
# Reactive Functions -----------------------------------------------------
  drawdown_inputs <- eventReactive(input$drawdown_resim, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),
  
  drawdown_simulations_reactive <- eventReactive(input$drawdown_resim, {
    drawdown_inputs = drawdown_inputs()
    return(Drawdown_Simulations(retire_age = drawdown_inputs$drawdown_retire_age, 
                                start_capital = drawdown_inputs$drawdown_start_capital, 
                                withdraw_freq = drawdown_inputs$drawdown_withdraw_freq, 
                                annual_mean_return = drawdown_inputs$drawdown_annual_mean_return, 
                                annual_ret_std_dev = drawdown_inputs$drawdown_annual_ret_std_dev, 
                                annual_inflation = drawdown_inputs$drawdown_annual_inflation, 
                                annual_inf_std_dev = drawdown_inputs$drawdown_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = drawdown_inputs$drawdown_withdraw_type, 
                                annual_withdrawals = drawdown_inputs$drawdown_annual_withdrawals, 
                                percent_withdrawal = drawdown_inputs$drawdown_percent_withdrawal))
  }, ignoreNULL = FALSE),
  
  drawdown_paths_reactive <- eventReactive(input$drawdown_resim, {
    return(Drawdown_Paths(drawdown_simulations_reactive()))
  }, ignoreNULL = FALSE),

  drawdown_withdrawals_reactive <- eventReactive(input$drawdown_resim, {
    return(Drawdown_Withdrawals(drawdown_simulations_reactive()))
  }, ignoreNULL = FALSE),

# Output Functions --------------------------------------------------------
  output$drawdown_text_life_ex <- renderText({
    drawdown_inputs = drawdown_inputs()
    ex = exn(ILT15_female_reduced, drawdown_inputs$drawdown_retire_age)
    return(c(round_2d(ex), " Years"))
  }),
  
  output$drawdown_text_average_fund_life_ex <- renderText({
    drawdown_inputs = drawdown_inputs()
    average = Drawdown_Mean_Life_Ex(Drawdown_Paths = drawdown_paths_reactive(),
                                    freq = drawdown_inputs$drawdown_withdraw_freq,
                                    age = drawdown_inputs$drawdown_retire_age)
    return(c("€", round_2d(average)))
  }),
  
  output$drawdown_text_ruin_prob_life_ex <- renderText({
    drawdown_inputs = drawdown_inputs()
    ruin = 100 * Drawdown_Ruin_Life_Ex(Drawdown_Paths = drawdown_paths_reactive(),
                                       freq = drawdown_inputs$drawdown_withdraw_freq,
                                       age = drawdown_inputs$drawdown_retire_age)
    return(c(round_2d(ruin), "%"))
  }),
  
  output$drawdown_table <- renderDataTable({
    drawdown_inputs = drawdown_inputs()
    freq = p_list[match(drawdown_inputs$drawdown_withdraw_freq, freq_list)]
    series = c(1, (freq * seq(1, (length(drawdown_paths_reactive()[1, ]) / freq), 1)) + 1)
    return(Drawdown_Table(Drawdown_Paths = drawdown_paths_reactive(),
                          Drawdown_Withdrawals = drawdown_withdrawals_reactive(),
                          freq = drawdown_inputs$drawdown_withdraw_freq, 
                          series = series))
  }),
  
  output$drawdown_plot_sims <- renderPlot({
    return(Drawdown_Plot_Sims(Drawdown_Paths = drawdown_paths_reactive(), n_sims = 25))
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$drawdown_withdraw_type,{
    if(input$drawdown_withdraw_type == T) {
      updateNumericInputIcon(session, "drawdown_percent_withdrawal", value = 4)
      enable("drawdown_percent_withdrawal")
      disable("drawdown_annual_withdrawals")
      updateNumericInputIcon(session, "drawdown_annual_withdrawals", value = NA)
    } else {
      updateNumericInputIcon(session, "drawdown_annual_withdrawals", value = 15000)
      enable("drawdown_annual_withdrawals")
      disable("drawdown_percent_withdrawal")
      updateNumericInputIcon(session, "drawdown_percent_withdrawal", value = NA)
    }
  })
)