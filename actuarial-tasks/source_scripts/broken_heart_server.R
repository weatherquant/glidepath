list(
# Reactive Functions -----------------------------------------------------
  bh_inputs <- eventReactive(input$bh_resim, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),
  
  bh_simulations_reactive <- eventReactive(input$bh_resim, {
    bh_inputs = bh_inputs()
    return(Drawdown_Simulations(retire_age = bh_inputs$bh_age_current, 
                                start_capital = bh_inputs$bh_start_capital, 
                                withdraw_freq = bh_inputs$bh_withdraw_freq, 
                                annual_mean_return = bh_inputs$bh_annual_mean_return, 
                                annual_ret_std_dev = bh_inputs$bh_annual_ret_std_dev, 
                                annual_inflation = bh_inputs$bh_annual_inflation, 
                                annual_inf_std_dev = bh_inputs$bh_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = bh_inputs$bh_withdraw_type, 
                                annual_withdrawals = bh_inputs$bh_annual_withdrawals, 
                                percent_withdrawal = bh_inputs$bh_percent_withdrawal,
                                life_table = BrokenHeart_LifeTable(gender = bh_inputs$bh_gender)))
  }, ignoreNULL = FALSE),
  
  bh_paths_reactive <- eventReactive(input$bh_resim, {
    return(Drawdown_Paths(bh_simulations_reactive()))
  }, ignoreNULL = FALSE),

  bh_withdrawals_reactive <- eventReactive(input$bh_resim, {
    return(Drawdown_Withdrawals(bh_simulations_reactive()))
  }, ignoreNULL = FALSE),

  bh_life_ex_widowed_reactive <- eventReactive(input$bh_resim, {
    bh_inputs = bh_inputs()
    return(exn(BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = bh_inputs$bh_age_widowed, gender = bh_inputs$bh_gender), bh_inputs$bh_age_current))
  }, ignoreNULL = FALSE),

  bh_life_ex_not_widowed_reactive <- eventReactive(input$bh_resim, {
    bh_inputs = bh_inputs()
    return(exn(BrokenHeart_LifeTable(widowed_status = FALSE, gender = bh_inputs$bh_gender), bh_inputs$bh_age_current))
  }, ignoreNULL = FALSE),

# Output Functions --------------------------------------------------------
  output$bh_text_life_ex_widowed <- renderText({
    return(c(round_2d(bh_life_ex_widowed_reactive()), " Years"))
  }),

  output$bh_text_life_ex_not_widowed <- renderText({
    return(c(round_2d(bh_life_ex_not_widowed_reactive()), " Years"))
  }),  
  
  output$bh_text_life_ex_diff <- renderText({
    diff = bh_life_ex_not_widowed_reactive() - bh_life_ex_widowed_reactive()
    return(c(round_2d(diff), " Years"))
  }),
  
  output$bh_text_average_fund_life_ex_widowed <- renderText({
    bh_inputs = bh_inputs()
    average = Drawdown_Mean_Life_Ex(Drawdown_Paths = bh_paths_reactive(),
                                    freq = bh_inputs$bh_withdraw_freq,
                                    ex = bh_life_ex_widowed_reactive())
    return(c("€", round_2d(average)))
  }),

  output$bh_text_average_fund_life_ex_not_widowed <- renderText({
    bh_inputs = bh_inputs()
    average = Drawdown_Mean_Life_Ex(Drawdown_Paths = bh_paths_reactive(),
                                    freq = bh_inputs$bh_withdraw_freq,
                                    ex = bh_life_ex_not_widowed_reactive())
    return(c("€", round_2d(average)))
  }),

  output$bh_text_ruin_prob_life_ex_widowed <- renderText({
    bh_inputs = bh_inputs()
    ruin = 100 * Drawdown_Ruin_Life_Ex(Drawdown_Paths = bh_paths_reactive(),
                                       freq = bh_inputs$bh_withdraw_freq,
                                       ex = bh_life_ex_widowed_reactive())
    return(c(round_2d(ruin), "%"))
  }),

  output$bh_text_ruin_prob_life_ex_not_widowed <- renderText({
    bh_inputs = bh_inputs()
    ruin = 100 * Drawdown_Ruin_Life_Ex(Drawdown_Paths = bh_paths_reactive(),
                                       freq = bh_inputs$bh_withdraw_freq,
                                       ex = bh_life_ex_not_widowed_reactive())
    return(c(round_2d(ruin), "%"))
  }),
  
  output$bh_text_total_withdrawals_life_ex_widowed <- renderText({
    bh_inputs = bh_inputs()
    total_withdrawals = Drawdown_Total_Withdrawals_Life_Ex(Drawdown_Withdrawals = bh_withdrawals_reactive(),
                                                           freq = bh_inputs$bh_withdraw_freq,
                                                           ex = bh_life_ex_widowed_reactive())
    return(c("€", round_2d(total_withdrawals)))
  }),

  output$bh_text_total_withdrawals_life_ex_not_widowed <- renderText({
    bh_inputs = bh_inputs()
    total_withdrawals = Drawdown_Total_Withdrawals_Life_Ex(Drawdown_Withdrawals = bh_withdrawals_reactive(),
                                                           freq = bh_inputs$bh_withdraw_freq,
                                                           ex = bh_life_ex_not_widowed_reactive())
    return(c("€", round_2d(total_withdrawals)))
  }),
  
  output$bh_table <- renderDataTable({
    bh_inputs = bh_inputs()
    freq = p_list[match(bh_inputs$bh_withdraw_freq, freq_list)]
    series = c(1, (freq * seq(1, (length(bh_paths_reactive()[1, ]) / freq), 1)) + 1)
    points = (freq * c(round_to_fraction(bh_life_ex_widowed_reactive(), freq), round_to_fraction(bh_life_ex_not_widowed_reactive(), freq))) + 1
    colour = c('yellow', 'orange')
    return(Drawdown_Table(Drawdown_Paths = bh_paths_reactive(),
                          Drawdown_Withdrawals = bh_withdrawals_reactive(),
                          freq = bh_inputs$bh_withdraw_freq, 
                          series = series,
                          points = points,
                          colour = colour))
  }),

  output$bh_plot_sims <- renderPlot({
    return(Drawdown_Plot_Sims(Drawdown_Paths = bh_paths_reactive(), n_sims = 25))
  }),

  output$bh_qx_change_plot <- renderPlotly({
    bh_inputs = bh_inputs()
    bh_w_f = BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = bh_inputs$bh_age_widowed, gender = 1)
    bh_no_f = BrokenHeart_LifeTable(widowed_status = FALSE, widowed_age = bh_inputs$bh_age_widowed, gender = 1)
    bh_w_m = BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = bh_inputs$bh_age_widowed, gender = 2)
    bh_no_m = BrokenHeart_LifeTable(widowed_status = FALSE, widowed_age = bh_inputs$bh_age_widowed, gender = 2)
    bh_w_f_qx = bh_no_f_qx = bh_w_m_qx = bh_no_m_qx = numeric(getOmega(bh_w_f))
    
    for(i in 1:getOmega(bh_w_f) + 1){
      bh_w_f_qx[i] = (bh_w_f@lx[i] - bh_w_f@lx[i + 1])/bh_w_f@lx[i]
      bh_no_f_qx[i] = (bh_no_f@lx[i] - bh_no_f@lx[i + 1])/bh_no_f@lx[i]
      bh_w_m_qx[i] = (bh_w_m@lx[i] - bh_w_m@lx[i + 1])/bh_w_m@lx[i]
      bh_no_m_qx[i] = (bh_no_m@lx[i] - bh_no_m@lx[i + 1])/bh_no_m@lx[i]
    }
    
    df = data.frame(age = (bh_inputs$bh_age_widowed - 1):(bh_inputs$bh_age_widowed + 4), 
                    female_widowed = bh_w_f_qx[(bh_inputs$bh_age_widowed):(bh_inputs$bh_age_widowed + 5)],
                    female_baseline = bh_no_f_qx[(bh_inputs$bh_age_widowed):(bh_inputs$bh_age_widowed + 5)],
                    male_widowed = bh_w_m_qx[(bh_inputs$bh_age_widowed):(bh_inputs$bh_age_widowed + 5)],
                    male_baseline = bh_no_m_qx[(bh_inputs$bh_age_widowed):(bh_inputs$bh_age_widowed + 5)]
    )
  
    p <- ggplot(df, aes(x = age)) + xlab("Age") + ylab("Probability of Death (qx rate)") +
      geom_line(aes(y = female_widowed, colour = 'Female', linetype = 'Widowed', size = "Female")) + 
      geom_line(aes(y = female_baseline, colour = 'Female', linetype = 'Baseline',size = "Female")) + 
      geom_line(aes(y = male_widowed, colour = "Male", linetype = 'Widowed', size = "Male")) + 
      geom_line(aes(y = male_baseline, colour = "Male", linetype = "Baseline", size = "Male")) +
      scale_color_manual("Gender", values = c('hotpink3', "#4A8DBF")) +
      scale_linetype_manual("Widowhood Status", values = c("Widowed" = 1, "Baseline" = 2)) +
      scale_size_manual("Gender", values = c(1,1)) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = 'top', legend.box = "horizontal") +
      guides(color = guide_legend(override.aes = list(size = .5), shape = guide_legend(override.aes = list(size = 4))))

    ggplotly(p, tooltip = c("age", "female_widowed", "female_baseline", "male_widowed", "male_baseline")) %>%
      layout(legend = list(orientation = "h", y = 1.2))
  }),

  output$bh_life_ex_change_plot <- renderPlotly({
    bh_inputs = bh_inputs()
    brokenheart_lifetable_widowed = BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = bh_inputs$bh_age_widowed, gender = bh_inputs$bh_gender)
    brokenheart_lifetable_not_widowed = BrokenHeart_LifeTable(widowed_status = FALSE, gender = bh_inputs$bh_gender)
    bh_widowed_qx = bh_not_widowed_qx = numeric(getOmega(brokenheart_lifetable_widowed))
    for(i in 1:getOmega(brokenheart_lifetable_widowed) + 1){
      bh_widowed_qx[i] = (brokenheart_lifetable_widowed@lx[i] - brokenheart_lifetable_widowed@lx[i + 1])/brokenheart_lifetable_widowed@lx[i]
      bh_not_widowed_qx[i] = (brokenheart_lifetable_not_widowed@lx[i] - brokenheart_lifetable_not_widowed@lx[i + 1])/brokenheart_lifetable_not_widowed@lx[i]
    }
    diff_qx = ((bh_widowed_qx - bh_not_widowed_qx) * 100)/bh_not_widowed_qx
    df = data.frame(age = bh_inputs$bh_age_widowed:(bh_inputs$bh_age_widowed + 4), diff_qx = diff_qx[(bh_inputs$bh_age_widowed + 1):(bh_inputs$bh_age_widowed + 5)])
    fig <- plot_ly(df, x = ~age, y = ~diff_qx, type = 'bar',
                   hovertemplate = paste("%{xaxis.title.text}: %{x}<br>",
                                         "%{yaxis.title.text}: %{y}<br>",
                                         '<extra></extra>'))
    fig <- fig %>%
      layout(xaxis = list(title = "Age"), yaxis = list(title = "% increase in qx rate"))
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$bh_withdraw_type,{
    if(input$bh_withdraw_type == T) {
      updateNumericInputIcon(session, "bh_percent_withdrawal", value = 4)
      enable("bh_percent_withdrawal")
      disable("bh_annual_withdrawals")
      updateNumericInputIcon(session, "bh_annual_withdrawals", value = NA)
    } else {
      updateNumericInputIcon(session, "bh_annual_withdrawals", value = 15000)
      enable("bh_annual_withdrawals")
      disable("bh_percent_withdrawal")
      updateNumericInputIcon(session, "bh_percent_withdrawal", value = NA)
    }
  })
)