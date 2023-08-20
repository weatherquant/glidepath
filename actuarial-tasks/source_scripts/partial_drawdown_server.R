list(
# Input Function ----------------------------------------------------------
  pd_inputs <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),
  
# Reactive Functions - Annuities ------------------------------------------
  pd_annuity_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(SORP_Annuity(age_1 = pd_inputs$pd_age[1], 
                        relationship = pd_inputs$pd_relationship, 
                        freq = freq_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]))
  }, ignoreNULL = FALSE),
  
  pd_annuity_buylater_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(SORP_Annuity(age_1 = pd_inputs$pd_age[1], 
                        age_2 = pd_inputs$pd_age[2], 
                        relationship = pd_inputs$pd_relationship, 
                        freq = freq_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)],
                        guaranteed = 0))
  }, ignoreNULL = FALSE),
  
  pd_annuity_deferred_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(SORP_Annuity(age_1 = pd_inputs$pd_age[1], 
                        age_2 = pd_inputs$pd_age[2], 
                        relationship = pd_inputs$pd_relationship, 
                        freq = freq_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)],
                        guaranteed = 0,
                        deferred = T))
  }, ignoreNULL = FALSE),

  pd_annuity_payment_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(SORP_Pension_Payment(SORP_Fund = pd_inputs$pd_starting_capital, 
                                SORP_Annuity = pd_annuity_reactive(), 
                                freq = freq_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]))
  }, ignoreNULL = FALSE),

  pd_mean_annuity_payment_buylater_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    payment = SORP_Pension_Payment(SORP_Fund = pd_mean_fund_buylater_end_reactive(), 
                                   SORP_Annuity = pd_annuity_buylater_reactive(), 
                                   freq = freq_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)])
    return(SORP_Discount(x = payment,
                         age_1 = pd_inputs$pd_age[1], 
                         age_2 = pd_inputs$pd_age[2]))
  }, ignoreNULL = FALSE),

  pd_annuity_cost_deferred_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    freq = p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]
    annuity = pd_annuity_deferred_reactive()
    annuity_payment = pd_annuity_payment_reactive()
    return(annuity * annuity_payment * freq)
  }, ignoreNULL = FALSE),

# Reactive Functions - Drawdown --------------------------------------------
  pd_simulations_drawdown_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(Drawdown_Simulations(retire_age = pd_inputs$pd_age[1], 
                                start_capital = pd_inputs$pd_starting_capital, 
                                withdraw_freq = pd_inputs$pd_withdraw_freq, 
                                annual_mean_return = pd_inputs$pd_annual_mean_return, 
                                annual_ret_std_dev = pd_inputs$pd_annual_ret_std_dev, 
                                annual_inflation = pd_inputs$pd_annual_inflation, 
                                annual_inf_std_dev = pd_inputs$pd_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = pd_inputs$pd_withdraw_type_drawdown, 
                                annual_withdrawals = pd_inputs$pd_annual_withdrawals_drawdown, 
                                percent_withdrawal = pd_inputs$pd_percent_withdrawal_drawdown))
  }, ignoreNULL = FALSE),

  pd_paths_drawdown_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(Drawdown_Paths(pd_simulations_drawdown_reactive()))
  }, ignoreNULL = FALSE),

  pd_withdrawals_drawdown_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(Drawdown_Withdrawals(pd_simulations_drawdown_reactive()))
  }, ignoreNULL = FALSE),  

  pd_simulations_buylater_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(Drawdown_Simulations(retire_age = pd_inputs$pd_age[1], 
                                start_capital = pd_inputs$pd_starting_capital, 
                                withdraw_freq = pd_inputs$pd_withdraw_freq, 
                                annual_mean_return = pd_inputs$pd_annual_mean_return, 
                                annual_ret_std_dev = pd_inputs$pd_annual_ret_std_dev, 
                                annual_inflation = pd_inputs$pd_annual_inflation, 
                                annual_inf_std_dev = pd_inputs$pd_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = pd_inputs$pd_withdraw_type_buylater, 
                                annual_withdrawals = pd_inputs$pd_annual_withdrawals_buylater, 
                                percent_withdrawal = pd_inputs$pd_percent_withdrawal_buylater,
                                end_age = pd_inputs$pd_age[2] + 1))
  }, ignoreNULL = FALSE),

  pd_paths_buylater_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(Drawdown_Paths(pd_simulations_buylater_reactive()))
  }, ignoreNULL = FALSE),

  pd_withdrawals_buylater_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(Drawdown_Withdrawals(pd_simulations_buylater_reactive()))
  }, ignoreNULL = FALSE), 

  pd_mean_fund_buylater_end_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    time = p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)] * (input$pd_age[2] + 1 - input$pd_age[1])
    mean = Drawdown_Mean(Drawdown_Paths = pd_paths_buylater_reactive(), time = time)
  }, ignoreNULL = FALSE), 

  pd_simulations_deferred_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(Drawdown_Simulations(retire_age = pd_inputs$pd_age[1], 
                                start_capital = pd_inputs$pd_starting_capital - pd_annuity_cost_deferred_reactive(), 
                                withdraw_freq = pd_inputs$pd_withdraw_freq, 
                                annual_mean_return = pd_inputs$pd_annual_mean_return, 
                                annual_ret_std_dev = pd_inputs$pd_annual_ret_std_dev, 
                                annual_inflation = pd_inputs$pd_annual_inflation, 
                                annual_inf_std_dev = pd_inputs$pd_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = pd_inputs$pd_withdraw_type_deferred, 
                                annual_withdrawals = pd_inputs$pd_annual_withdrawals_deferred, 
                                percent_withdrawal = pd_inputs$pd_percent_withdrawal_deferred,
                                end_age = pd_inputs$pd_age[2] + 1))
  }, ignoreNULL = FALSE),

  pd_paths_deferred_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(Drawdown_Paths(pd_simulations_deferred_reactive()))
  }, ignoreNULL = FALSE),

  pd_withdrawals_deferred_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    return(Drawdown_Withdrawals(pd_simulations_deferred_reactive()))
  }, ignoreNULL = FALSE), 

  pd_mean_fund_deferred_end_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    time = p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)] * (input$pd_age[2] + 1 - input$pd_age[1])
    mean = Drawdown_Mean(Drawdown_Paths = pd_paths_deferred_reactive(), time = time)
  }, ignoreNULL = FALSE), 

  pd_life_ex_reactive <- eventReactive({input$pd_resim1; input$pd_resim2}, {
    pd_inputs = pd_inputs()
    return(round_to_fraction(exn(ILT15_female_reduced, pd_inputs$pd_age[1]), p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]))
  }, ignoreNULL = FALSE),

# Output Functions - Annuities --------------------------------------------
  output$pd_text_annuity_payment <- output$pd_text_annuity_payment_deferred <- renderText({
    return(c("$", round_2d(pd_annuity_payment_reactive(), T)))
  }),  
  
  output$pd_text_annuity_cumulative_life_ex <- renderText({
    pd_inputs = pd_inputs()
    freq = p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]
    cumulative = SORP_Cumulative_Payments(SORP_Pension_Payment = pd_annuity_payment_reactive(),
                                          time = freq * pd_life_ex_reactive())
    return(c("$", round_2d(cumulative, T)))
  }),

  output$pd_text_mean_annuity_payment_buylater <- renderText({
    return(c("$", round_2d(pd_mean_annuity_payment_buylater_reactive(), T)))
  }),  

  output$pd_text_annuity_cost_deferred <- renderText({
    return(c("$", round_2d(pd_annuity_cost_deferred_reactive(), T)))
  }),

# Output Functions - Drawdowns --------------------------------------------
  output$pd_text_drawdown_mean_withdrawals_life_ex <- renderText({
    pd_inputs = pd_inputs()
    mean = Drawdown_Mean_Withdrawals_Life_Ex(Drawdown_Withdrawals = pd_withdrawals_drawdown_reactive(),
                                                 freq = pd_inputs$pd_withdraw_freq,
                                                 ex = pd_life_ex_reactive())
    return(c("$", round_2d(mean, T)))
  }),

  output$pd_text_drawdown_mean_fund_life_ex <- renderText({
    pd_inputs = pd_inputs()
    mean = Drawdown_Mean_Life_Ex(Drawdown_Paths = pd_paths_drawdown_reactive(),
                                    freq = pd_inputs$pd_withdraw_freq,
                                    ex = pd_life_ex_reactive())
    return(c("$", round_2d(mean, T)))
  }),
  
  output$pd_text_buylater_mean_fund_end <- renderText({
    return(c("$", round_2d(pd_mean_fund_buylater_end_reactive(), T)))
  }),

  output$pd_text_deferred_mean_fund_end <- renderText({
    return(c("$", round_2d(pd_mean_fund_deferred_end_reactive(), T)))
  }),

# Output Functions - Table and Plot ------------------------------------------------
  output$pd_comparison_table <- renderDataTable({
    pd_inputs = pd_inputs()
    freq = p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]
    series = list(1, (freq * seq(5, (length(pd_paths_drawdown_reactive()[1, ]) / freq), 5)) + 1)
    points = list(pd_inputs$pd_age[2] - pd_inputs$pd_age[1], pd_life_ex_reactive())
    colour = list('orange', 'yellow')
    return(Partial_Drawdown_Comparison_Table(age_1 = pd_inputs$pd_age[1], 
                                             age_2 = pd_inputs$pd_age[2], 
                                             freq = pd_inputs$pd_withdraw_freq, 
                                             annuity_payment = pd_annuity_payment_reactive(), 
                                             Drawdown_Paths = pd_paths_drawdown_reactive(), 
                                             Drawdown_Withdrawals = pd_withdrawals_drawdown_reactive(),
                                             buy_later_mean_annuity_payment = pd_mean_annuity_payment_buylater_reactive(), 
                                             BuyLater_Paths = pd_paths_buylater_reactive(), 
                                             BuyLater_Withdrawals = pd_withdrawals_buylater_reactive(),
                                             Deferred_Paths = pd_paths_deferred_reactive(),
                                             Deferred_Withdrawals = pd_withdrawals_deferred_reactive(),
                                             series = series, 
                                             points = points, 
                                             colour = colour))
  }),

  output$pd_plot_income_compare <- renderPlotly({
    pd_inputs = pd_inputs()
    freq = p_list[match(pd_inputs$pd_withdraw_freq, freq_list_drawdown)]
    
    table_df = Partial_Drawdown_DataFrame(age_1 = pd_inputs$pd_age[1], 
                                          age_2 = pd_inputs$pd_age[2], 
                                          freq = pd_inputs$pd_withdraw_freq, 
                                          annuity_payment = pd_annuity_payment_reactive(), 
                                          Drawdown_Paths = pd_paths_drawdown_reactive(), 
                                          Drawdown_Withdrawals = pd_withdrawals_drawdown_reactive(),
                                          buy_later_mean_annuity_payment = pd_mean_annuity_payment_buylater_reactive(), 
                                          BuyLater_Paths = pd_paths_buylater_reactive(), 
                                          BuyLater_Withdrawals = pd_withdrawals_buylater_reactive(),
                                          Deferred_Paths = pd_paths_deferred_reactive(),
                                          Deferred_Withdrawals = pd_withdrawals_deferred_reactive())

    fig <- plot_ly(table_df, x = ~round(table_df$years, 2), y = ~round(table_df$annuity_total_paid), name = "100% Annuity", type = "scatter", mode = "lines", line = list(color = '#E52C89', width = 4))
    fig <- fig %>% add_trace(y = ~round(table_df$drawdown_mean_withdrawn), name = '100% Drawdown', line = list(color = '#31A6CD', width = 4))
    fig <- fig %>% add_trace(y = ~round(table_df$buy_later_total), name = 'Drawdown and Annuity Purchase', line = list(color = '#5f3787', width = 4))
    fig <- fig %>% add_trace(y = ~round(table_df$deferred_total), name = 'Drawdown and Deferred Annuity', line = list(color = '#59B252', width = 4))
    fig <- fig %>% layout(shapes = list(plotly_vline(x = pd_inputs$pd_age[2] - pd_inputs$pd_age[1], colour = 'orange'), plotly_vline(x = pd_life_ex_reactive(), colour = 'black')))
    fig <- fig %>% layout(xaxis = list(title = "Years Since Retirement"), yaxis = list(title = list(text = "Retirement Income", standoff = 15), tickprefix = '$', tickformat = ",.", ticklabelposition = "inside"))  
    fig <- fig %>% layout(legend = list(orientation = "h", xanchor = "center", yanchor = "bottom", x = 0.5, y = 1.2))
    fig <- fig %>% layout(hovermode = "x unified")
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$pd_withdraw_type_drawdown, {
    if(input$pd_withdraw_type_drawdown == T) {
      shinyjs::hide("pd_annual_withdrawals_drawdown_input")
      shinyjs::show("pd_percent_withdrawal_drawdown_input")
      updateNumericInputIcon(session, "pd_percent_withdrawal_drawdown", value = 4)
      enable("pd_percent_withdrawal_drawdown")
      disable("pd_annual_withdrawals_drawdown")
      updateNumericInputIcon(session, "pd_annual_withdrawals_drawdown", value = NA)
    } else {
      shinyjs::show("pd_annual_withdrawals_drawdown_input")
      shinyjs::hide("pd_percent_withdrawal_drawdown_input")
      updateNumericInputIcon(session, "pd_annual_withdrawals_drawdown", value = 10000)
      enable("pd_annual_withdrawals_drawdown")
      disable("pd_percent_withdrawal_drawdown")
      updateNumericInputIcon(session, "pd_percent_withdrawal_drawdown", value = NA)
    }
  }),
  
  observeEvent(input$pd_withdraw_type_buylater, {
    if(input$pd_withdraw_type_buylater == T) {
      shinyjs::hide("pd_annual_withdrawals_buylater_input")
      shinyjs::show("pd_percent_withdrawal_buylater_input")
      updateNumericInputIcon(session, "pd_percent_withdrawal_buylater", value = 4)
      enable("pd_percent_withdrawal_buylater")
      disable("pd_annual_withdrawals_buylater")
      updateNumericInputIcon(session, "pd_annual_withdrawals_buylater", value = NA)
    } else {
      shinyjs::show("pd_annual_withdrawals_buylater_input")
      shinyjs::hide("pd_percent_withdrawal_buylater_input")
      updateNumericInputIcon(session, "pd_annual_withdrawals_buylater", value = 10000)
      enable("pd_annual_withdrawals_buylater")
      disable("pd_percent_withdrawal_buylater")
      updateNumericInputIcon(session, "pd_percent_withdrawal_buylater", value = NA)
    }
  }),

  observeEvent(input$pd_withdraw_type_deferred, {
    if(input$pd_withdraw_type_deferred == T) {
      shinyjs::hide("pd_annual_withdrawals_deferred_input")
      shinyjs::show("pd_percent_withdrawal_deferred_input")
      updateNumericInputIcon(session, "pd_percent_withdrawal_deferred", value = 4)
      enable("pd_percent_withdrawal_deferred")
      disable("pd_annual_withdrawals_deferred")
      updateNumericInputIcon(session, "pd_annual_withdrawals_deferred", value = NA)
    } else {
      shinyjs::show("pd_annual_withdrawals_deferred_input")
      shinyjs::hide("pd_percent_withdrawal_deferred_input")
      updateNumericInputIcon(session, "pd_annual_withdrawals_deferred", value = 10000)
      enable("pd_annual_withdrawals_deferred")
      disable("pd_percent_withdrawal_deferred")
      updateNumericInputIcon(session, "pd_percent_withdrawal_deferred", value = NA)
    }
  })
)