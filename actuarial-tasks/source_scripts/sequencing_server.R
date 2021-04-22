list(
  Seq_Sim <- function(seq_start_capital, seq_withdraw_freq, seq_annual_mean_return, seq_worst_annual_return, seq_best_annual_return, seq_annual_inflation, seq_n_years, seq_withdraw_type = F, seq_annual_withdrawals, seq_percent_withdrawal = 4){
    
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
    if (seq_withdraw_type == F) {
      periodic.withdrawals = seq_annual_withdrawals / p
    } else {
      periodic.withdrawals = start.capital * effective2Convertible(i=seq_percent_withdrawal/100,k=p)/p
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
    
    Spaths = matrix(0, 3, seq_n_obs)
    withdrawals_matrix = matrix(0, 3, seq_n_obs)
    
    periodic.invest.returns = matrix(0, 3, seq_n_obs)
    periodic.invest.returns[1,] = rep(seq(periodic.worst.return,periodic.best.return,length.out = seq_n_years), each=p)
    periodic.invest.returns[2,] = rep(periodic.mean.return,seq_n_obs)
    periodic.invest.returns[3,] = rep(seq(periodic.best.return,periodic.worst.return,length.out = seq_n_years), each=p)
    
    if(periodic.withdrawals > start.capital){
      Spaths[, 1] = 0
      withdrawals_matrix[, 1] = start.capital
      mylist = list(Spaths, withdrawals_matrix)
      return(mylist)
    }
    
    Spaths[,1] = start.capital - periodic.withdrawals
    withdrawals_matrix[, 1] = periodic.withdrawals
    
    for(i in 1:3){
      next_fund_value = Spaths[i, 1] * (1 + periodic.invest.returns[i,1]) - ((1 + periodic.inflation) * periodic.withdrawals)
      if (next_fund_value <= 0) {
        withdrawals_matrix[i, 2] = Spaths[i, 1] * (1 + periodic.invest.returns[i,1])
        next
      }
      Spaths[i, 2] = next_fund_value
      withdrawals_matrix[i, 2] = ((1 + periodic.inflation) * periodic.withdrawals)
      for(j in 3:seq_n_obs){
        next_fund_value = Spaths[i, j - 1] * (1 + periodic.invest.returns[i, j - 1]) - ((1 + periodic.inflation)^(j-1) * periodic.withdrawals)
        if(next_fund_value <= 0){
          withdrawals_matrix[i, j] = Spaths[i, j - 1] * (1 + periodic.invest.returns[i,j - 1])
          break
        } else {
          Spaths[i, j] = next_fund_value
          withdrawals_matrix[i, j] = ((1 + periodic.inflation)^(j - 1) * periodic.withdrawals)
        }
      }
      
      # for(j in 1:seq_n_obs){
      #   if (seq_percent_yn == F) {
      #     if (Spaths[i,j] <= 0) {
      #       break
      #     } 
      #     else if (Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)-periodic.withdrawals <= 0) {
      #       break
      #     } 
      #     else {
      #       Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)-periodic.withdrawals
      #       periodic_withdrawals[i, j] = periodic.withdrawals
      #     }
      #   } 
      #   else {
      #     if (Spaths[i,j] <= 0) {
      #       break
      #     } else if (Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)*(1 - (periodic.percent.withdrawal * (1+periodic.inflation)^j)) <= 0) {
      #       break
      #     } else {
      #       Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)*(1 - (periodic.percent.withdrawal * (1+periodic.inflation)^j))
      #       periodic_withdrawals[i, j] = Spaths[i,j]*(1+periodic.invest.returns[i,j]-periodic.inflation)*(periodic.percent.withdrawal * (1+periodic.inflation)^j)
      #     }
      #   }
      # }
    }
    mylist = list(Spaths, withdrawals_matrix)
    return(mylist)
    #return(Spaths)
  },
  
  seq_drawdown_Paths <- function(Seq_Sim){
    return(Seq_Sim[[1]])
  },
  
  seq_drawdown_Withdrawals <- function(Seq_Sim){
    return(Seq_Sim[[2]])
  },
  
  #Reactive functions
  seq_inputs <- eventReactive({input$seq_resim1; input$seq_resim2}, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),
  
  
  sequencing_react <- reactive({
    validate(
      need(input$seq_start_capital != "" && input$seq_annual_mean_return != "" && input$seq_best_annual_return != "" &&
             input$seq_annual_inflation != "" && input$seq_n_years != "" && (input$seq_annual_withdrawals != "" || input$seq_percent_withdrawal!= ""), 
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
                   seq_annual_withdrawals = input$seq_annual_withdrawals,
                   seq_withdraw_type = input$seq_withdraw_type))
  }),
  
  
  seq_paths_reactive <- eventReactive({input$seq_resim1; input$seq_resim2}, {
    return(seq_drawdown_Paths(sequencing_react()))
  }, ignoreNULL = FALSE),
  
  seq_withdrawals_reactive <- eventReactive({input$seq_resim1; input$seq_resim2}, {
    return(seq_drawdown_Withdrawals(sequencing_react()))
  }, ignoreNULL = FALSE),
  
  euro <- dollar_format(prefix = "", suffix = "\u20ac"),
  
  output$sequencing_sim_plot_three <- renderPlotly({
    seq_inputs = seq_inputs()
    p = p_list[match(seq_inputs$seq_withdraw_freq, freq_list)]
    # number of periods to simulate
    seq_n_obs = p * seq_inputs$seq_n_years
    Spaths <- seq_paths_reactive()
    
    years = c(seq(0, ((length(Spaths[1, ]) - 1) / p), by = 1/p))
    
    df = data.frame(Years = rep(years,3),
                    Capital = c(round(Spaths[1,],2), round(Spaths[2,],2), round(Spaths[3,],2)),
                    Market = c(rep("Bad Early Years", seq_n_obs),
                               rep("Average Early Years", seq_n_obs),
                               rep("Good Early Years", seq_n_obs)))
    graph = ggplot(data = df, aes(x = Years, y = Capital)) +
      geom_line(aes(color = Market), size = 1) +
      theme_classic() + 
      #geom_point(aes(color = Market)) +
      scale_colour_manual(values = c("#00A7FA", "#FF3B28", "#48EB12")) +
      ylab("Capital") +
      scale_y_continuous(labels = dollar_format(suffix = "", prefix = "€"), expand = c(0, 0), limits = c(0,max(Spaths)*1.1)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) +
      theme(
        # legend.position = c(.95, .50),
        # legend.justification = c("right", "top"),
        # legend.box.just = "right",
        # legend.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_line(size = 1, linetype = 'solid',
                                        colour = "#ECECEC")
      )
    
    fig <- ggplotly(graph)
    fig <- fig %>% layout(xaxis = list(title = "Years"), yaxis = list(title = list(text = "Capital", standoff = 15)))  
    fig <- fig %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.15), showlegend = T)
    fig <- fig %>% layout(hovermode = "x unified")
    return(fig)
  }),
  
  output$seq_drawdown_table <- DT::renderDataTable({
    seq_inputs = seq_inputs()
    p = p_list[match(seq_inputs$seq_withdraw_freq, freq_list)]
    seq_n_obs = p * seq_inputs$seq_n_years
    Spaths <- seq_paths_reactive()
    
    df.table = data.frame(Time = 0:(seq_n_obs-1),
                          Col_1 =Spaths[1, ],
                          Col_2 = Spaths[2, ],
                          Col_3 = Spaths[3, ]
    )
    
    colnames(df.table) = c("Time", "Bad Early Years", "Average Early Years", "Good Early Years")
    table <- datatable(df.table, options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
    table <- formatCurrency(table, columns = c("Bad Early Years", "Average Early Years", "Good Early Years"), currency = "€")
    table <- formatStyle(table, 1, `border-right` = "solid 1px")
    table <- formatStyle(table, c(2,3), `border-right` = "dotted 1px")
    ?formatStyle
    return(table)
  }),
  
  #Observe Functions
  observeEvent(input$seq_withdraw_type,{
    if(input$seq_withdraw_type == T) {
      shinyjs::hide("seq_annual_withdrawals_input")
      shinyjs::show("seq_percent_withdrawal_input")
      updateNumericInputIcon(session, "seq_percent_withdrawal", value = 4)
      enable("seq_percent_withdrawal")
      disable("seq_annual_withdrawals")
      updateNumericInputIcon(session, "seq_annual_withdrawals", value = NA)
    } else {
      shinyjs::show("seq_annual_withdrawals_input")
      shinyjs::hide("seq_percent_withdrawal_input")
      updateNumericInputIcon(session, "seq_annual_withdrawals", value = 30000)
      enable("seq_annual_withdrawals")
      disable("seq_percent_withdrawal")
      updateNumericInputIcon(session, "seq_percent_withdrawal", value = NA)
    }
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