list(
  loanSummary <- reactive(
    {
      PV = input$PV
      int = input$int / 100
      n = input$n
      freq = input$freq
      
      p = p_list[match(input$freq, freq_list)]
      ip_p = effective2Convertible(int, k = p)/p
      
      total_payments = n * p
      repay = PV/annuity(i = int, n = n, k = p)/p
      
      repay_no = seq(0, total_payments)
      repay_vect = c(0, rep(repay, total_payments))
      balance = numeric(total_payments + 1)
      int_paid = numeric(total_payments + 1)
      cap_paid = numeric(total_payments + 1)
      int_percent = numeric(total_payments + 1)
      cap_percent = numeric(total_payments + 1)
      
      balance[1] = PV
      
      for(i in (1:(total_payments))){
        int_paid[i + 1] = ip_p * balance[i]
        cap_paid[i + 1] =  repay - int_paid[i + 1]
        balance[i + 1] = balance[i] - cap_paid[i + 1]
        int_percent[i + 1] = (int_paid[i + 1]) / repay 
        cap_percent[i + 1] = (cap_paid[i + 1]) / repay 
      }
      
      loan_summary <- data.frame(repay_no, balance, int_percent, cap_percent, int_paid, cap_paid, repay_vect)
      return(loan_summary)
    }
  ),
  
  output$repay <- renderText({
    loan_summary = loanSummary()
    repayment = loan_summary[2, 7]
    return(c("€", format(round(as.numeric(repayment), 2), nsmall = 2, big.mark = ",", scientific=FALSE)))
  }),
  
  output$repay_box <- renderInfoBox({
    loan_summary = loanSummary()
    repayment = paste((c("€", format(round(as.numeric(loan_summary[2, 7]), 2), nsmall = 2, big.mark = ",", scientific=FALSE))), collapse = "")
    infoBox(
      "Periodic Repayment Amount", repayment, icon = icon("euro")
    )
  }),
  
  output$loan_balance <- renderPlot({
    loan_summary = loanSummary()
    ggplot(loan_summary, aes(x = repay_no, y = balance, fill="#4A8DBF", color="#4A8DBF")) + geom_bar(stat='identity', color = "#4A8DBF", fill = "#4A8DBF") + labs(x = "Repayment Number", y = "Remaining Balance") + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
  }),
  
  output$int_cap <- renderPlot({
    loan_summary = loanSummary()
    loan_summary[, 3:4] = 100*loan_summary[, 3:4]
    colnames(loan_summary) = c("repay_no", 'balance', "Interest", "Capital", 'int_paid', 'cap_paid', 'repay_vect')
    int_cap <- data.frame(pivot_longer(select(loan_summary, -repay_vect), Interest:Capital, names_to = "int_or_cap", values_to = "value"))
    ggplot(int_cap, aes(x=repay_no, y=value, fill=int_or_cap)) + geom_bar(stat="identity") + scale_fill_manual(values = c("#4A8DBF", "#BF7C4A")) + labs(x = "Repayment Number", y = "Proportion of Repayment", fill = NULL) + scale_x_continuous(limits = c(0, nrow(loan_summary)), expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))
  }),
  
  output$loan_schedule <- renderDataTable({
    loan_summary = loanSummary()
    loan_summary_tidy <- data.frame(select(loan_summary, -repay_no, -repay_vect), row.names = loan_summary[,1])
    colnames(loan_summary_tidy) = c("Balance", "% Interest", "% Capital", "Interest Paid", "Capital Paid")
    ls <- datatable(loan_summary_tidy, options = list(paging = FALSE, searching = FALSE)) 
    ls <- formatCurrency(ls, columns = c(1, 4, 5), currency = "€")
    ls <- formatPercentage(ls, columns = c(2, 3), digits = 2)
    return(ls)
  })
)