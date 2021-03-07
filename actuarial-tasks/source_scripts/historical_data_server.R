list(
  output$finalfundtime <- renderText({
    #wts.timelimit = input$wts_timelimit
    #time.period = as.integer((input$date_end-input$date_start)/365)*12 + round((as.double(input$date_end-input$date_start)%%365)/30,0)
    #m = format( as.Date(input$date_start) %m+% months(time.taken + offset_check()))
    #message = paste0(min(wts.timelimit, time.period), " months")
    message = get_date()
    return(message)
  }),
  
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "Close", icon = icon("info-circle"))
      )
    ))
  }),
  
  observeEvent(input$intro,{
    removeModal()
  }),
  
  drawdown_react <- reactive({
    hist_drawdown_index_df()
  }),
  
  hist_drawdown_index_df <- function(){
    
    validate(
      need(input$date_start < input$date_end, "Please ensure the start date does not exceed the end date")
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
    index_df_weighted = matrix(0,n_obs,n_weights)
    index_df_weighted[1,] = t(wts*t(index_df.returns[1,]))
    weighted_returns = numeric(n_obs)
    weighted_returns[1] = sum(index_df_weighted[1,])
    Spaths = matrix(0, n_weights+1, n_obs+1)
    periodic_withdrawals = numeric(n_obs)
    Spaths[,1] = c(wts*start.capital,start.capital)
    #periodic.inflation.returns = numeric(n_obs) #use real data
    periodic.inflation = index_df_inflation #use real data
    periodic.withdrawals = input$annual_withdrawals/12
    offset = input$offset
    
    #time moves from left to right in Spaths!
    for(j in 1:n_obs){
      if (Spaths[n_weights+1,j]*(1+weighted_returns[j]-periodic.inflation[j])-periodic.withdrawals <= 0) {
        break
      }
      else {
        Spaths[n_weights+1,j+1] = Spaths[n_weights+1,j]*(1+weighted_returns[j]-periodic.inflation[j])-periodic.withdrawals
        periodic_withdrawals[j] = periodic.withdrawals
        for(k in 1:n_weights){
          Spaths[k,j+1] = wts[k]*Spaths[n_weights+1,j+1]
        }
        if(j <= wts.timelimit + offset && (j > offset && wts.timelimit !=0)){
          wts = wts + wts_increase
        }
        if(j < n_obs){
          #find weighted returns with new weights for next year
          index_df_weighted[j+1,] = t(wts*t(index_df.returns[j+1,]))
          for(i in 1:n_weights){
            weighted_returns[j+1] = sum(index_df_weighted[j+1,])}
        }
      }
    }
    time_date = seq(as.Date(input$date_start), as.Date(input$date_end), by="month")
    Spaths_array = round(c(Spaths[n_weights+1,]),2)
    index_df_plot = data.frame(time = time_date, capital = Spaths_array)
    return(index_df_plot)
  },
  
  output$hist_drawdown <- renderPlot({
    index_df_plot = hist_drawdown_index_df()
    g <- ggplot(data = index_df_plot, aes(x = time, y = capital) ) +
      geom_line() +
      theme_economist()
    return(g)
  }),
  
  offset_check <- function(){
    if(input$wts_timelimit == 0){
      offset_check = 0
    }
    else{
      offset_check = input$offset
    }
  },
  
  get_date <- function(){
    wts_perc = c(input$stock,input$bond,input$gold,input$realt,input$rfr)
    wts_increase_perc = c(input$stock_inc,input$bond_inc,input$gold_inc,input$realt_inc,input$rfr_inc)
    wts.timelimit = input$wts_timelimit
    time.period = as.integer((input$date_end-input$date_start)/365)*12 + round((as.double(input$date_end-input$date_start)%%365)/30,0)
    if(wts.timelimit < time.period){
      final.fund = wts_perc + wts_increase_perc*wts.timelimit
      time.taken = wts.timelimit
    }
    else{
      final.fund = wts_perc + wts_increase_perc*time.period
      time.taken = time.period
    }
    
    m = format( as.Date(input$date_start) %m+% months(time.taken + offset_check()))
    return(m)
  },
  
  output$final_fund <- renderPlot(final_fund_index_df()),
  
  final_fund_index_df <- function(){
    
    validate(
      need(input$date_start < input$date_end, "Please ensure the start date does not exceed the end date")
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
    
    wts_perc = c(input$stock,input$bond,input$gold,input$realt,input$rfr)
    wts_increase_perc = c(input$stock_inc,input$bond_inc,input$gold_inc,input$realt_inc,input$rfr_inc)
    wts.timelimit = input$wts_timelimit
    time.period = as.integer((input$date_end-input$date_start)/365)*12 + round((as.double(input$date_end-input$date_start)%%365)/30,0)
    if(wts.timelimit < time.period){
      final.fund = wts_perc + wts_increase_perc*wts.timelimit
      time.taken = wts.timelimit
    }
    else{
      final.fund = wts_perc + wts_increase_perc*time.period
      time.taken = time.period
    }
    
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