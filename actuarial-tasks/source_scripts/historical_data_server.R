list(
  historical_inputs <- eventReactive({input$hist_resim1; input$hist_resim2; input$hist_resim3}, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),
  
  output$finalfundtime <- renderText({
    message = get_date()
    return(message)
  }),
  
  
  historical_reactive <- eventReactive({input$hist_resim1; input$hist_resim2; input$hist_resim3}, {
    #historical_inputs = historical_inputs()
    return(hist_drawdown_index_df())
  }, ignoreNULL = FALSE),
  
  # drawdown_react <- reactive({
  #   hist_drawdown_index_df()
  # }),
  
  hist_drawdown_index_df <- function(){
    validate(
      need(input$date_start < input$date_end, "Please ensure the start date does not exceed the end date")
    )
    validate(
      need(length(seq(as.Date(input$date_start), as.Date(input$date_end)%m-%months(1), by="month")) > 1, "Please ensure the simulation lasts for more than one month")
    )
    validate(
      need(input$stock+input$bond+input$gold+input$realt+input$rfr == 100,
           "Please ensure portfolio allocations add up to 100%")
    )
    validate(
      need(input$stock_inc+input$bond_inc+input$gold_inc+input$realt_inc+input$rfr_inc == 0,
           "Please ensure future changes in portfolio allocations add up to 0")
    )
    validate(
      need(input$offset != "",
           "Please provide a time to offset adjustments by (if you do not want adjustments to occur, please input 0)")
    )
    validate(
      need(input$wts_timelimit != "",
           "Please provide a time limit for adjustments to take effect for (if you do not want adjustments to occur, please input 0)")
    )
    validate(
      need(0 <= input$stock + input$stock_inc*input$wts_timelimit &&
             input$stock + input$stock_inc*input$wts_timelimit <= 100 &&
             0 <= input$bond + input$bond_inc*input$wts_timelimit &&
             input$bond + input$bond_inc*input$wts_timelimit <= 100 &&
             0 <= input$gold + input$gold_inc*input$wts_timelimit &&
             input$gold + input$gold_inc*input$wts_timelimit <= 100 &&
             0 <= input$realt + input$realt_inc*input$wts_timelimit &&
             input$realt + input$realt_inc*input$wts_timelimit <= 100 &&
             0 <= input$rfr + input$rfr_inc*input$wts_timelimit &&
             input$rfr + input$rfr_inc*input$wts_timelimit <= 100,
           "Please ensure portfolio allocations will never go below 0 or exceed 100")
    )
    
    validate(
      need((as.integer((input$date_end-input$date_start)/365)*12 + round((as.double(input$date_end-input$date_start)%%365)/30,0)) > input$offset,
           "Please ensure that the adjustments start date is within the selected time period")
    )
    
    validate(
      need(input$annual_withdrawals/12 <= input$start_capital,
           "Please ensure that the starting capital is high enough to make a withdrawal")
    )
    
    index_df.date = index_df %>% dplyr::filter(date >= input$date_start & date < input$date_end)
    index_df.returns = index_df.date[,-1]
    index_df_inflation = index_df_inflation %>% dplyr::filter(date >= input$date_start & date < input$date_end)
    index_df_inflation = index_df_inflation[,-1]
    wts = c(input$stock,input$bond,input$gold,input$realt,input$rfr)/100 #must add up to 100% & be > 0
    wts_increase = c(input$stock_inc,input$bond_inc,input$gold_inc,input$realt_inc,input$rfr_inc)/100 #must add up to 0
    wts.timelimit = input$wts_timelimit #must not be so long as to allow any weights to be <0
    #need code in rshiny to calculate one from the other - time from value & value from time
    
    start.capital = input$start_capital
    n_obs = nrow(index_df.returns) #no. months (time periods)
    n_weights = length(wts)
    index_df_weighted = matrix(0,n_obs-1,n_weights)
    index_df_weighted[1,] = t(wts*t(index_df.returns[1,]))
    weighted_returns = numeric(n_obs-1)
    weighted_returns[1] = sum(index_df_weighted[1,])
    
    Spaths = matrix(0, n_weights+1, n_obs)
    periodic_withdrawals = numeric(n_obs-1)
    #periodic.inflation.returns = numeric(n_obs) #use real data
    periodic.inflation = index_df_inflation #use real data
    periodic.withdrawals = input$annual_withdrawals/12
    offset = input$offset
    
    Spaths[,1] = c(wts*(start.capital-periodic.withdrawals), start.capital-periodic.withdrawals)
    
    #time moves from left to right in Spaths!
    #j=1
    if (Spaths[n_weights+1,1]*(1+weighted_returns[1])-periodic.withdrawals*(1+periodic.inflation[1]) <= 0) {
      time_date = seq(as.Date(input$date_start), as.Date(input$date_end)%m-%months(1), by="month")
      Spaths_array = round(c(Spaths[n_weights+1,]),2)
      index_df_plot = data.frame(Time = time_date, Capital = Spaths_array)
      return(index_df_plot)
    }
    else {
      Spaths[n_weights+1,2] = Spaths[n_weights+1,1]*(1+weighted_returns[1])-periodic.withdrawals*(1+periodic.inflation[1])
      #periodic_withdrawals[1] = periodic.withdrawals*(1+periodic.inflation[1])
      for(k in 1:n_weights){
        Spaths[k,2] = wts[k]*Spaths[n_weights+1,2]
      }
      if(1 <= wts.timelimit + offset && (1 > offset && wts.timelimit !=0)){
        wts = wts + wts_increase
      }
      if(1 < (n_obs-1)){
        #find weighted returns with new weights for next year
        index_df_weighted[2,] = t(wts*t(index_df.returns[2,]))
        for(i in 1:n_weights){
          weighted_returns[2] = sum(index_df_weighted[2,])}
      }
    }
    
    if(length(seq(as.Date(input$date_start), as.Date(input$date_end)%m-%months(1), by="month")) > 2){
    #j=2,3,...
    for(j in 2:(n_obs-1)){
      if (Spaths[n_weights+1,j]*(1+weighted_returns[j])-periodic.withdrawals*prod(1+periodic.inflation[1:j]) <= 0) {
        break
      }
      else {
        Spaths[n_weights+1,j+1] = Spaths[n_weights+1,j]*(1+weighted_returns[j])-periodic.withdrawals*prod(1+periodic.inflation[1:j])
        #periodic_withdrawals[j] = periodic.withdrawals*prod(1+periodic.inflation[1:j])
        for(k in 1:n_weights){
          Spaths[k,j+1] = wts[k]*Spaths[n_weights+1,j+1]
        }
        if(j <= wts.timelimit + offset && (j > offset && wts.timelimit !=0)){
          wts = wts + wts_increase
        }
        if(j < (n_obs-1)){
          #find weighted returns with new weights for next year
          index_df_weighted[j+1,] = t(wts*t(index_df.returns[j+1,]))
          for(i in 1:n_weights){
            weighted_returns[j+1] = sum(index_df_weighted[j+1,])}
        }
      }
    }
    }
    time_date = seq(as.Date(input$date_start), as.Date(input$date_end)%m-%months(1), by="month")
    Spaths_array = round(c(Spaths[n_weights+1,]),2)
    index_df_plot = data.frame(Time = time_date, Capital = Spaths_array)
    return(index_df_plot)
  },
  
  output$hist_drawdown <- renderPlotly({
    index_df_plot = historical_reactive()
    g <- ggplot(data = index_df_plot, aes(x = Time, y = Capital) ) +
      geom_line() +
      scale_y_continuous(labels = dollar_format(suffix = "", prefix = "$"), limits=c(0,NA)) + 
      theme_economist()
    fig <- ggplotly(g)
    return(fig)
  }),
  
  # timelimit_check <- function(){
  #   if(input$wts_timelimit == FALSE){
  #     timelimit_check = 0
  #   }
  #   else{
  #     timelimit_check = input$wts_timelimit
  #   }
  # },
  
  offset_check <- function(){
    if(input$wts_timelimit == 0){
      offset_check = 0
    }
    else{
      offset_check = input$offset
    }
  },
  
  get_date <- function(){
    req(input$wts_timelimit, input$offset, input$date_start, input$date_end)
    req(input$stock,input$bond,input$gold,input$realt,input$rfr,input$stock_inc,input$bond_inc,input$gold_inc,input$realt_inc,input$rfr_inc,
        cancelOutput = TRUE)
    wts_perc = c(input$stock,input$bond,input$gold,input$realt,input$rfr)
    wts_increase_perc = c(input$stock_inc,input$bond_inc,input$gold_inc,input$realt_inc,input$rfr_inc)
    wts.timelimit = input$wts_timelimit
    wts.offset = offset_check()
    time.period = as.integer((input$date_end-input$date_start)/365)*12 + round((as.double(input$date_end-input$date_start)%%365)/30,0)
    if(input$stock_inc ==0 && input$bond_inc ==0 && input$gold_inc ==0 && input$realt_inc ==0 && input$rfr_inc ==0){
      final.fund = wts_perc
      time.taken = 0
    }
    else if(0 > input$stock + input$stock_inc*wts.timelimit ||
            input$stock + input$stock_inc*wts.timelimit > 100 ||
            0 > input$bond + input$bond_inc*wts.timelimit ||
            input$bond + input$bond_inc*wts.timelimit > 100 ||
            0 > input$gold + input$gold_inc*wts.timelimit ||
            input$gold + input$gold_inc*wts.timelimit > 100 ||
            0 > input$realt + input$realt_inc*wts.timelimit ||
            input$realt + input$realt_inc*wts.timelimit > 100 ||
            0 > input$rfr + input$rfr_inc*wts.timelimit ||
            input$rfr + input$rfr_inc*wts.timelimit > 100){
      return("")
    }
    else if(wts.timelimit + wts.offset < time.period){
      final.fund = wts_perc + wts_increase_perc*wts.timelimit
      time.taken = wts.timelimit + wts.offset
    }
    else{
      final.fund = wts_perc + wts_increase_perc*(time.period - wts.offset)
      time.taken = time.period
    }
    
    m = format( as.Date(input$date_start) %m+% months(time.taken))
    return(m)
  },
  
  output$final_fund <- renderPlot({
    historical_inputs = historical_inputs()
    return(final_fund_index_df(historical_inputs$date_start, 
                               historical_inputs$date_end, 
                               historical_inputs$stock, 
                               historical_inputs$bond, 
                               historical_inputs$gold, 
                               historical_inputs$realt, 
                               historical_inputs$rfr, 
                               historical_inputs$stock_inc, 
                               historical_inputs$bond_inc, 
                               historical_inputs$gold_inc, 
                               historical_inputs$realt_inc, 
                               historical_inputs$rfr_inc, 
                               historical_inputs$wts_timelimit,
                               historical_inputs$offset, 
                               historical_inputs$annual_withdrawals, 
                               historical_inputs$start_capital))
  }),
  
  final_fund_index_df <- function(date_start, date_end, stock, bond, gold, realt, rfr, 
                                  stock_inc, bond_inc, gold_inc, realt_inc, rfr_inc, wts_timelimit,
                                  offset, annual_withdrawals, start_capital){
    
    validate(
      need(date_start < date_end, 
           "Please ensure the start date does not exceed the end date")
    )
    validate(
      need(length(seq(as.Date(date_start), as.Date(date_end)%m-%months(1), by="month")) > 1, "Please ensure the simulation lasts for more than one month")
    )
    validate(
      need(stock+bond+gold+realt+rfr == 100,
           "Please ensure portfolio allocations add up to 100%")
    )
    validate(
      need(stock_inc+bond_inc+gold_inc+realt_inc+rfr_inc == 0,
           "Please ensure future changes in portfolio allocations add up to 0")
    )
    validate(
      need(offset != "",
      "Please provide a time to offset adjustments by (if you do not want adjustments to occur, please input 0)")
    )
    validate(
      need(wts_timelimit != "",
      "Please provide a time limit for adjustments to take effect for (if you do not want adjustments to occur, please input 0)")
    )
    validate(
      need(0 <= stock + stock_inc*wts_timelimit &&
             stock + stock_inc*wts_timelimit <= 100 &&
             0 <= bond + bond_inc*wts_timelimit &&
             bond + bond_inc*wts_timelimit <= 100 &&
             0 <= gold + gold_inc*wts_timelimit &&
             gold + gold_inc*wts_timelimit <= 100 &&
             0 <= realt + realt_inc*wts_timelimit &&
             realt + realt_inc*wts_timelimit <= 100 &&
             0 <= rfr + rfr_inc*wts_timelimit &&
             rfr + rfr_inc*wts_timelimit <= 100,
           "Please ensure portfolio allocations will never go below 0 or exceed 100")
    )
    
    validate(
      need((as.integer((date_end-date_start)/365)*12 + round((as.double(date_end-date_start)%%365)/30,0)) > offset,
           "Please ensure that the adjustments start date is within the selected time period")
    )
    
    validate(
      need(annual_withdrawals/12 < start_capital,
           "Please ensure that the starting capital is high enough to make a withdrawal")
    )
    
    wts_perc = c(stock, bond, gold, realt, rfr)
    wts_increase_perc = c(stock_inc, bond_inc, gold_inc, realt_inc, rfr_inc)
    wts.timelimit = wts_timelimit
    wts.offset = offset
    time.period = as.integer((date_end-date_start)/365)*12 + round((as.double(date_end-date_start)%%365)/30,0)
    if(wts.timelimit + wts.offset < time.period){
      final.fund = wts_perc + wts_increase_perc*wts.timelimit
      time.taken = wts.timelimit
    }
    else{
      final.fund = wts_perc + wts_increase_perc*(time.period - wts.offset)
      time.taken = time.period - wts.offset
    }
    
    
    # wts_perc = c(input$stock,input$bond,input$gold,input$realt,input$rfr)
    # wts_increase_perc = c(input$stock_inc,input$bond_inc,input$gold_inc,input$realt_inc,input$rfr_inc)
    # wts.timelimit = input$wts_timelimit
    # time.period = as.integer((input$date_end-input$date_start)/365)*12 + round((as.double(input$date_end-input$date_start)%%365)/30,0)
    # if(wts.timelimit < time.period){
    #   final.fund = wts_perc + wts_increase_perc*wts.timelimit
    #   time.taken = wts.timelimit
    # }
    # else{
    #   final.fund = wts_perc + wts_increase_perc*time.period
    #   time.taken = time.period
    # }
    
    data <- data.frame(
      Category=c("Stocks", "Bonds", "Gold", "Real Estate", "Risk-free Rate"),
      count=c(final.fund[1], final.fund[2], final.fund[3],final.fund[4], final.fund[5])
    )
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- label_percent()(data$count/100)
    
    # Make the plot
    g <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
      geom_rect() +
      geom_text(x=4.2, aes(y=labelPosition, label=label), size=4) + 
      #geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
      scale_fill_brewer(palette=1) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "right")
    
    return(g)
  }
)