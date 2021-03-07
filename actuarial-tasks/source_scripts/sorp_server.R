list(
# Reactive Functions ------------------------------------------------------
  sorp_annuity_reactive <- reactive({
    return(SORP_Annuity(age_1 = input$sorp_age[1], 
                        age_2 = input$sorp_age[2], 
                        relationship = input$sorp_relationship, 
                        freq = input$sorp_post_freq, 
                        annuity_interest = input$sorp_annuity_interest, 
                        annuity_esc = input$sorp_annuity_esc, 
                        guaranteed = input$sorp_guaranteed))
  }),
  
  sorp_contributions_reactive <- reactive({
    return(SORP_Contributions(age_1 = input$sorp_age[1], 
                              age_2 = input$sorp_age[2], 
                              salary = input$sorp_salary, 
                              current_fundvalue = input$sorp_current_fundvalue, 
                              freq = input$sorp_pre_freq, 
                              emp_contri = input$sorp_emp_contri, 
                              empr_contri = input$sorp_empr_contri, 
                              salary_esc = input$sorp_salary_esc,
                              investment_charges = input$sorp_investment_charges,
                              equity_prop = input$sorp_equity_prop,
                              equity_rate = input$sorp_equity_rate,
                              fixed_prop = input$sorp_fixed_prop, 
                              fixed_rate = input$sorp_fixed_rate, 
                              cash_prop = input$sorp_cash_prop, 
                              cash_rate = input$sorp_cash_rate))
  }),
  
  sorp_fund_reactive <- reactive({
    return(SORP_Fund(SORP_Contributions = sorp_contributions_reactive()))
  }),
  
  sorp_pension_payment_reactive <- reactive({
    return(SORP_Pension_Payment(SORP_Fund = sorp_fund_reactive(), 
                                SORP_Annuity = sorp_annuity_reactive(), 
                                freq = input$sorp_post_freq))
  }),

# Output Functions --------------------------------------------------------
  output$sorp_text_fundvalue <- renderText({
    return(c("€", round_2d(sorp_fund_reactive(), T)))
  }),
  
  output$sorp_text_pension_payment <- renderText({
    return(c("€", round_2d(sorp_pension_payment_reactive(), T)))
  }),
  
  output$sorp_text_fundvalue_discounted <- renderText({
    discounted_fund = SORP_Discount(x = sorp_fund_reactive(),
                           age_1 = input$sorp_age[1], 
                           age_2 = input$sorp_age[2],
                           discount_rate = input$sorp_discount_rate)
    return(c("€", round_2d(discounted_fund, T)))
  }),
  
  output$sorp_text_pension_payment_discounted <- renderText({
    discounted_pension_payment = SORP_Discount(x = sorp_pension_payment_reactive(),
                                      age_1 = input$sorp_age[1], 
                                      age_2 = input$sorp_age[2],
                                      discount_rate = input$sorp_discount_rate)
    return(c("€", round_2d(discounted_pension_payment, T)))
  }),

  output$sorp_plot_fundvalue <- renderPlot({
    SORP_Plot_FundValue(SORP_Contributions = sorp_contributions_reactive(), freq = input$sorp_pre_freq)
  }),
  
  output$sorp_table_contributions <- renderDataTable({
    SORP_Table_Contributions(SORP_Contributions = sorp_contributions_reactive(), freq = input$sorp_pre_freq)
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$sorp_default, {
    updateNumericInputIcon(session, "sorp_salary_esc", value = 1.5)
    updateNumericInputIcon(session, "sorp_discount_rate", value = 2.5)
    updateNumericInputIcon(session, "sorp_annuity_interest", value = 0.5)
    updateNumericInputIcon(session, "sorp_annuity_esc", value = 1)
    updateNumericInputIcon(session, "sorp_guaranteed", value = 5)
    updateNumericInputIcon(session, "sorp_investment_charges", value = 0.5)
    updateSliderInput(session, "sorp_equity_prop", value = 40)
    updateSliderInput(session, "sorp_equity_rate", value = 4.5)
    updateSliderInput(session, "sorp_fixed_prop", value = 30)
    updateSliderInput(session, "sorp_fixed_rate", value = 1)
    updateSliderInput(session, "sorp_cash_prop", value = 30)
    updateSliderInput(session, "sorp_cash_rate", value = 0)
  }),

  observeEvent(input$sorp_equity_prop,{
    updateSliderInput(session, "sorp_fixed_prop", max = 100 - input$sorp_equity_prop)
    disable("sorp_cash_prop") # putting this here keeps the slider disabled all the time (but still shows updating)
  }),

  observe({
    updateSliderInput(session, "sorp_cash_prop", value = 100 - input$sorp_equity_prop - input$sorp_fixed_prop)
  }),

  observeEvent(input$sorp_age[2],{
    updateNumericInputIcon(session, "sorp_guaranteed", max = getOmega(ILT15_female_reduced) - input$sorp_age[2])
  })
)