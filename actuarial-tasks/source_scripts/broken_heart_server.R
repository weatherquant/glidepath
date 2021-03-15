list(
# Reactive Functions -----------------------------------------------------
  bh_life_ex_widowed_reactive <- reactive({
    ex = exn(BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = input$bh_age_widowed, gender = input$bh_gender), input$bh_age_current)
    return(ex)
  }),

  bh_life_ex_not_widowed_reactive <- reactive({
    ex = exn(BrokenHeart_LifeTable(widowed_status = FALSE, gender = input$bh_gender), input$bh_age_current)
    return(ex)
  }),

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

  output$bh_qx_change_plot <- renderPlotly({
    bh_w_f = BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = input$bh_age_widowed, gender = 1)
    bh_no_f = BrokenHeart_LifeTable(widowed_status = FALSE, widowed_age = input$bh_age_widowed, gender = 1)
    bh_w_m = BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = input$bh_age_widowed, gender = 2)
    bh_no_m = BrokenHeart_LifeTable(widowed_status = FALSE, widowed_age = input$bh_age_widowed, gender = 2)
    bh_w_f_qx = bh_no_f_qx = bh_w_m_qx = bh_no_m_qx = numeric(getOmega(bh_w_f))
    
    for(i in 1:getOmega(bh_w_f) + 1){
      bh_w_f_qx[i] = (bh_w_f@lx[i] - bh_w_f@lx[i + 1])/bh_w_f@lx[i]
      bh_no_f_qx[i] = (bh_no_f@lx[i] - bh_no_f@lx[i + 1])/bh_no_f@lx[i]
      bh_w_m_qx[i] = (bh_w_m@lx[i] - bh_w_m@lx[i + 1])/bh_w_m@lx[i]
      bh_no_m_qx[i] = (bh_no_m@lx[i] - bh_no_m@lx[i + 1])/bh_no_m@lx[i]
    }
    
    df = data.frame(age = (input$bh_age_widowed - 1):(input$bh_age_widowed + 4), 
                    female_widowed = bh_w_f_qx[(input$bh_age_widowed):(input$bh_age_widowed + 5)],
                    female_baseline = bh_no_f_qx[(input$bh_age_widowed):(input$bh_age_widowed + 5)],
                    male_widowed = bh_w_m_qx[(input$bh_age_widowed):(input$bh_age_widowed + 5)],
                    male_baseline = bh_no_m_qx[(input$bh_age_widowed):(input$bh_age_widowed + 5)]
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
    brokenheart_lifetable_widowed = BrokenHeart_LifeTable(widowed_status = TRUE, widowed_age = input$bh_age_widowed, gender = input$bh_gender)
    brokenheart_lifetable_not_widowed = BrokenHeart_LifeTable(widowed_status = FALSE, gender = input$bh_gender)
    bh_widowed_qx = bh_not_widowed_qx = numeric(getOmega(brokenheart_lifetable_widowed))
    for(i in 1:getOmega(brokenheart_lifetable_widowed) + 1){
      bh_widowed_qx[i] = (brokenheart_lifetable_widowed@lx[i] - brokenheart_lifetable_widowed@lx[i + 1])/brokenheart_lifetable_widowed@lx[i]
      bh_not_widowed_qx[i] = (brokenheart_lifetable_not_widowed@lx[i] - brokenheart_lifetable_not_widowed@lx[i + 1])/brokenheart_lifetable_not_widowed@lx[i]
    }
    diff_qx = ((bh_widowed_qx - bh_not_widowed_qx) * 100)/bh_not_widowed_qx
    df = data.frame(age = input$bh_age_widowed:(input$bh_age_widowed + 4), diff_qx = diff_qx[(input$bh_age_widowed + 1):(input$bh_age_widowed + 5)])
    fig <- plot_ly(df, x = ~age, y = ~diff_qx, type = 'bar',
                   hovertemplate = paste("%{xaxis.title.text}: %{x}<br>",
                                         "%{yaxis.title.text}: %{y}<br>",
                                         '<extra></extra>'))
    fig <- fig %>%
      layout(xaxis = list(title = "Age"), yaxis = list(title = "% increase in qx rate"))
  })
)