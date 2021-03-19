list(
  loan_reactive <- reactive({
    return(loan_summary(inital_balance = input$loan_inital_balance, 
                        interest_rate = input$loan_interest,
                        term_years = input$loan_years,
                        freq_repay = input$loan_freq))
    }),
  
  output$loan_periodic_repay <- renderText({
    loan_summary = loan_reactive()
    repayment = loan_summary$repay_vect[2]
    return(c("€", round_2d(repayment, T)))
  }),
  
  output$loan_schedule <- renderDataTable({
    loan_summary = loan_reactive()
    loan_summary[loan_summary == 0] <- "-"
    loan_summary <- data.frame(select(loan_summary, -repay_no, -repay_vect), row.names = loan_summary[,1])
    colnames(loan_summary) = c("Balance", "% Interest", "% Capital", "Interest Paid", "Capital Paid")
    loan_summary <- datatable(loan_summary, options = list(scrollX = TRUE, scrollY = "350px", paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all")))) 
    loan_summary <- formatCurrency(loan_summary, columns = c("Balance", "Interest Paid", "Capital Paid"), currency = "€")
    loan_summary <- formatPercentage(loan_summary, columns = c("% Interest", "% Capital"), digits = 2)
    return(loan_summary)
  }),
  
  output$loan_plot_balance <- renderPlot({
    loan_summary = loan_reactive()
    ggplot(loan_summary, aes(x = repay_no, y = balance, fill="#4A8DBF", color="#4A8DBF")) + 
      geom_bar(stat = 'identity', color = "#4A8DBF", fill = "#4A8DBF") + 
      labs(x = "Repayment Number", y = "Remaining Balance") + 
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(labels = scales::dollar_format(prefix = "€"), expand = c(0, 0)) + 
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
  }),
  
  output$loan_plot_interest_vs_capital <- renderPlot({
    loan_summary = loan_reactive()
    loan_summary = loan_summary[-1, ]
    colnames(loan_summary)[3:4] = c("Interest", "Capital")
    int_cap <- data.frame(pivot_longer(select(loan_summary, -repay_vect), Interest:Capital, names_to = "int_or_cap", values_to = "value"))
    ggplot(int_cap, aes(x=repay_no, y=value, fill = int_or_cap, colour = int_or_cap)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = c("#4A8DBF", "#BF7C4A")) + scale_colour_manual(values = c("#4A8DBF", "#BF7C4A"), guide = FALSE) +
      labs(x = "Repayment Number", y = "% of Repayment", fill = NULL) + xlim(1, nrow(loan_summary)) +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
      theme(legend.position = "top", legend.text = element_text(size = 11),
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
  })
)