list(
# Intro Pop-Up ------------------------------------------------------------
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "Close", icon = NULL)
      )
    ))
  }),

  observeEvent(input$intro,{
    removeModal()
  }),
  
# Loan Calculator Functions -----------------------------------------------
  loan_summary <- function(inital_balance, interest_rate, term_years, freq_repay){
    interest_rate = interest_rate / 100
    
    p = p_list[match(freq_repay, freq_list)]
    ip_p = effective2Convertible(interest_rate, k = p)/p
    
    total_payments = term_years * p
    repay = inital_balance / annuity(i = interest_rate, n = term_years, k = p) / p
    
    repay_no = seq(0, total_payments)
    repay_vect = c(0, rep(repay, total_payments)) 
    
    balance = numeric(total_payments + 1)
    int_paid = numeric(total_payments + 1)
    cap_paid = numeric(total_payments + 1)
    int_percent = numeric(total_payments + 1)
    cap_percent = numeric(total_payments + 1)
    
    balance[1] = inital_balance
    
    for(i in (1:(total_payments))){
      int_paid[i + 1] = ip_p * balance[i]
      cap_paid[i + 1] =  repay - int_paid[i + 1]
      balance[i + 1] = balance[i] - cap_paid[i + 1]
      int_percent[i + 1] = (int_paid[i + 1]) / repay 
      cap_percent[i + 1] = (cap_paid[i + 1]) / repay 
    }
    
    loan_summary <- data.frame(repay_no, balance, int_percent, cap_percent, int_paid, cap_paid, repay_vect)
    return(loan_summary)
  },

# SORP Functions ----------------------------------------------------------
  SORP_Annuity <- function(age_1, age_2 = age_1, relationship, freq, annuity_interest = 0.5, annuity_esc = 1, guaranteed = 5, deferred = F){
    freq = p_list[match(freq, freq_list)]
    
    net_interest = ((1 + (annuity_interest/100))/(1 + (annuity_esc/100))) - 1
    
    # annuities will increase by 0.33% per annum compound
    years_to_buy = as.numeric(format(Sys.Date(), "%Y")) + (age_2 - age_1)
    ann_inc_rate = 1.0033 ^ (years_to_buy - 2013)
    
    if(deferred == F){
      guar_ann = annuity(i = net_interest, n = guaranteed, k = freq, type = "advance")
      if (relationship == 1) {
        annuity = (guar_ann + axn(ILT15_female_reduced, x = age_2, i = net_interest, k = freq, m = guaranteed, payment = "advance")) * ann_inc_rate
      } else {
        annuity = (guar_ann + axyzn(listOfTables, x = c(age_2, age_2), i = net_interest, m = guaranteed, k = freq, status = "last", payment = "advance")) * ann_inc_rate
      }
    } else {
      if (relationship == 1) {
        annuity = axn(ILT15_female_reduced, x = age_1, i = net_interest, k = freq, m = (age_2 - age_1), payment = "advance") * ann_inc_rate
      } else {
        annuity = axyzn(listOfTables, x = c(age_1, age_1), i = net_interest, m = (age_2 - age_1), k = freq, status = "last", payment = "advance") * ann_inc_rate
      }
    }
    return(annuity)
  },
  
  SORP_Contributions <- function(age_1, age_2, salary, current_fundvalue, freq, emp_contri, empr_contri, salary_esc = 1.5, investment_charges = 0.5, equity_prop = 40, equity_rate = 4.5, fixed_prop = 30, fixed_rate = 1, cash_prop = 30, cash_rate = 0){
    
    freq = p_list[match(freq, freq_list)]
    
    contributions_interest_annual = ((equity_prop/100) * (equity_rate/100)) + ((fixed_prop/100) * (fixed_rate/100)) + ((cash_prop/100) * (cash_rate/100)) - (investment_charges/100)
    
    contributions_interest = effective2Convertible(i = contributions_interest_annual, k = freq) / freq
    
    fundValueX = numeric((age_2 - age_1)*freq + 1)
    ages = numeric((age_2 - age_1)*freq + 1)
    ages_exact = numeric((age_2 - age_1)*freq + 1)
    periods = numeric((age_2 - age_1)*freq + 1)
    EEContribution = numeric((age_2 - age_1)*freq + 1)
    ERContribution = numeric((age_2 - age_1)*freq + 1)
    totalContribution = numeric((age_2 - age_1)*freq + 1)
    interestOnFund = numeric((age_2 - age_1)*freq + 1)
    
    ages[1] = age_1 - 1
    ages_exact[1] = age_1 - 1/freq
    periods[1] = freq
    fundValueX[1] = current_fundvalue
    
    for (m in 2:((age_2 - age_1)*freq + 1)){
      ages[m] = ages[m-1]
      ages_exact[m] = ages_exact[m - 1] + 1 / freq
      periods[m] = periods[m - 1] + 1
      EEContribution[m] = salary * (emp_contri / 100) / freq
      ERContribution[m] = salary * (empr_contri / 100) / freq
      totalContribution[m] = EEContribution[m] + ERContribution[m]
      interestOnFund[m] = fundValueX[m - 1] * contributions_interest
      fundValueX[m] = fundValueX[m - 1] + interestOnFund[m] + totalContribution[m]
      
      if((m - 1)%%freq == 0){
        salary = salary * (1 + (salary_esc/100))
      }
      if((m - 2)%%freq == 0){
        periods[m] = 1
        ages[m] = ages[m - 1] + 1
      }
    }
    
    df_fund <- data.frame(age = ages, period = periods, age_exact = ages_exact,
                          EmployeeContribution = EEContribution,
                          EmployerContribution = ERContribution,
                          TotalContribution = totalContribution,
                          InterestOnFund = interestOnFund,
                          FundValue = fundValueX
    )
    return(df_fund)
  },
  
  SORP_Fund <- function(SORP_Contributions){
    fund_FV <- SORP_Contributions[length(SORP_Contributions[, "FundValue"]), "FundValue"]
    return(fund_FV)
  },

  SORP_Pension_Payment <- function(SORP_Fund, SORP_Annuity, freq){
    freq = p_list[match(freq, freq_list)]
    pension_payment = SORP_Fund / SORP_Annuity / freq
    return(pension_payment)
  },

  SORP_Discount <- function(x, age_1, age_2, discount_rate = 2.5){
    discount_factor = 1/((1 + discount_rate/100)^(age_2 - age_1))
    return(x * discount_factor)
  },

  SORP_Cumulative_Payments <- function(SORP_Pension_Payment, time){
    return(SORP_Pension_Payment * time)
  },

  SORP_Plot_FundValue <- function(SORP_Contributions, freq){
    freq = p_list[match(freq, freq_list)]
    AgeandFundValue <- SORP_Contributions %>% select(age_exact, FundValue)
    p <- ggplot(AgeandFundValue, aes(x = age_exact + (1 / freq), y = FundValue, fill = "#4A8DBF", colour = "#4A8DBF")) +
      geom_bar(stat = "identity", colour = "#4A8DBF", fill = "#4A8DBF") + 
      labs(x = "Age", y = "Fund Value", fill = NULL, color = NULL) +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(labels = scales::dollar_format(prefix = "€"), expand = c(0, 0)) + 
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    return(p)
  },

  SORP_Table_Contributions <- function(SORP_Contributions, freq){
    SORP_Contributions[1, 1:(length(SORP_Contributions[1, ]) - 1)] <- "-"
    freq = p_list[match(freq, freq_list)]
    if(freq == 1){
      SORP_Contributions = data.frame(select(SORP_Contributions, -period, -age_exact))
      colnames(SORP_Contributions) = c("Age", "Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period")
    } else {
      SORP_Contributions = data.frame(select(SORP_Contributions, -age_exact))
      colnames(SORP_Contributions) = c("Age", "Period", "Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period")
    }
    SORP_Contributions <- datatable(SORP_Contributions, options = list(scrollX = TRUE, scrollY = "300px", paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames= FALSE)
    SORP_Contributions <- formatCurrency(SORP_Contributions, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period"), currency = "€")
    return(SORP_Contributions)
  },

# Drawdown Functions ------------------------------------------------------
  Drawdown_Simulations <- function(retire_age, start_capital, withdraw_freq, annual_mean_return, annual_ret_std_dev, annual_inflation, annual_inf_std_dev, n_sim = 10000, withdraw_type = F, annual_withdrawals = 15000, percent_withdrawal = 4, life_table = ILT15_female_reduced, end_age = NA){
    
    p = p_list[match(withdraw_freq, freq_list_drawdown)]
    
    # Investment
    annual_mean_return = annual_mean_return / 100
    annual_ret_std_dev = annual_ret_std_dev / 100
    
    # Inflation
    annual_inflation = annual_inflation / 100
    annual_inf_std_dev = annual_inf_std_dev / 100
    
    # Withdrawals
    if (withdraw_type == F) {
      periodic_withdrawals = annual_withdrawals / p
    } else {
      periodic_withdrawals = start_capital * effective2Convertible(i = percent_withdrawal / 100, k = p) / p
    }
    
    # Time to Run
    if(is.na(end_age)){
      n_years = getOmega(life_table) - retire_age
    } else {
      n_years = end_age - retire_age
    }
    
    # number of periods to simulate
    n_obs = p * n_years
    
    # periodic Investment and Inflation assumptions
    periodic_mean_return = effective2Convertible(i=annual_mean_return,k=p)/p
    periodic_ret_std_dev = annual_ret_std_dev / sqrt(p)
    
    periodic_inflation = effective2Convertible(i=annual_inflation,k=p)/p
    periodic_inf_std_dev = annual_inf_std_dev / sqrt(p)
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    paths_matrix = matrix(0, n_sim, n_obs)
    withdrawals_matrix = matrix(0, n_sim, n_obs)
    
    periodic_invest_returns = numeric(n_obs - 1)
    periodic_inflation_returns = numeric(n_obs - 1)
    
    if(periodic_withdrawals > start_capital){
      paths_matrix[, 1] = 0
      withdrawals_matrix[, 1] = start_capital
      mylist = list(paths_matrix, withdrawals_matrix)
      return(mylist)
    }
    
    paths_matrix[, 1] = start_capital - periodic_withdrawals
    withdrawals_matrix[, 1] = periodic_withdrawals
    
    for(i in 1:n_sim){
      periodic_invest_returns = rnorm(n_obs - 1, mean = periodic_mean_return, sd = periodic_ret_std_dev)
      # To Not Adjust for Inflation on Fixed Withdrawals:
      # if (withdraw_type == F) {
      #   periodic_inflation_returns = numeric(n_obs - 1)
      # } else {
      #   periodic_inflation_returns = rnorm(n_obs - 1, mean = periodic_inflation, sd = periodic_inf_std_dev)
      # }
      periodic_inflation_returns = rnorm(n_obs - 1, mean = periodic_inflation, sd = periodic_inf_std_dev)
      
      next_fund_value = paths_matrix[i, 1] * (1 + periodic_invest_returns[1]) - ((1 + periodic_inflation_returns[1]) * periodic_withdrawals)
      if (next_fund_value <= 0) {
        withdrawals_matrix[i, 2] = paths_matrix[i, 1] * (1 + periodic_invest_returns[1])
        next
      }
      paths_matrix[i, 2] = next_fund_value
      withdrawals_matrix[i, 2] = ((1 + periodic_inflation_returns[1]) * periodic_withdrawals)
      
      for(j in 3:n_obs){
        next_fund_value = paths_matrix[i, j - 1] * (1 + periodic_invest_returns[j - 1]) - (prod(1 + periodic_inflation_returns[1:j - 1]) * periodic_withdrawals)
        if(next_fund_value <= 0){
          withdrawals_matrix[i, j] = paths_matrix[i, j - 1] * (1 + periodic_invest_returns[j - 1])
          break
        } else {
          paths_matrix[i, j] = next_fund_value
          withdrawals_matrix[i, j] = (prod(1 + periodic_inflation_returns[1:j - 1]) * periodic_withdrawals)
        }
      }
    }
    mylist = list(paths_matrix, withdrawals_matrix)
    return(mylist)
  },

  Drawdown_Paths <- function(Drawdown_Simulations){
    return(Drawdown_Simulations[[1]])
  },

  Drawdown_Withdrawals <- function(Drawdown_Simulations){
    return(Drawdown_Simulations[[2]])
  },
  
  Drawdown_Mean <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ])){
      return(mean(Drawdown_Paths[, time]))
  },

  Drawdown_Mean_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Mean(Drawdown_Paths, time))
  },

  Drawdown_Percentile <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ]), percentile = 0.50){
    return(quantile(Drawdown_Paths[, time], probs = percentile))
  },

  Drawdown_Percentile_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, percentile = 0.50, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Percentile(Drawdown_Paths, time, percentile))
  },

  Drawdown_Ruin_Prob <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ]), ruin_value = 0){
    return(length(which(Drawdown_Paths[, time] <= ruin_value)) / length(Drawdown_Paths[, time]))
  },

  Drawdown_Ruin_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced, ruin_value = 0){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Ruin_Prob(Drawdown_Paths, time, ruin_value))
  },

  Drawdown_Mean_Withdrawals <- function(Drawdown_Withdrawals, time = length(Drawdown_Withdrawals[1, ])){
    if(time == 1){
      return(mean(Drawdown_Withdrawals[, time]))
    } else {
      return(mean(rowSums(Drawdown_Withdrawals[, 1:time])))
    }
  },

  Drawdown_Mean_Withdrawals_Life_Ex <- function(Drawdown_Withdrawals, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Mean_Withdrawals(Drawdown_Withdrawals, time))
  },

  Drawdown_Percentile_Withdrawals <- function(Drawdown_Withdrawals, time = length(Drawdown_Withdrawals[1, ]), percentile = 0.50){
    if(time == 1){
      return(quantile(Drawdown_Withdrawals[, time], probs = percentile))
    } else {
      return(quantile(rowSums(Drawdown_Withdrawals[, 1:time]), probs = percentile))
    }
  },

  Drawdown_Percentile_Withdrawals_Life_Ex <- function(Drawdown_Withdrawals, freq, percentile = 0.50, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Percentile_Withdrawals(Drawdown_Withdrawals, time, percentile))
  },

  Drawdown_DataFrame <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, lower = 0.05, upper = 0.95, ruin_value = 0){
    freq = p_list[match(freq, freq_list_drawdown)]
  
    total_withdrawn = numeric(length(Drawdown_Withdrawals[1, ]))
    average = numeric(length(Drawdown_Paths[1, ]))
    prob_ruin = numeric(length(Drawdown_Paths[1, ]))
    lower_bound = numeric(length(Drawdown_Paths[1, ]))
    median = numeric(length(Drawdown_Paths[1, ]))
    upper_bound = numeric(length(Drawdown_Paths[1, ]))
    years = c(seq(0, ((length(Drawdown_Paths[1, ]) - 1) / freq), by = 1/freq))
  
    for(i in 1:length(Drawdown_Paths[1, ])){
      total_withdrawn[i] = Drawdown_Mean_Withdrawals(Drawdown_Withdrawals, i)
      average[i] = Drawdown_Mean(Drawdown_Paths, i)
      prob_ruin[i] = Drawdown_Ruin_Prob(Drawdown_Paths, i, ruin_value)
      lower_bound[i] = Drawdown_Percentile(Drawdown_Paths, i, percentile = lower)
      median[i] = Drawdown_Percentile(Drawdown_Paths, i)
      upper_bound[i] = Drawdown_Percentile(Drawdown_Paths, i, percentile = upper)
    }
    dataframe = data.frame(years, total_withdrawn, average, prob_ruin, lower_bound, median, upper_bound)
    return(dataframe)
  },

  Drawdown_Table <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, series = NULL, points = NULL, colour = NULL, lower = 0.05, upper = 0.95, ruin_value = 0){
    dataframe = Drawdown_DataFrame(Drawdown_Paths = Drawdown_Paths, Drawdown_Withdrawals = Drawdown_Withdrawals, freq = freq, lower = lower, upper = upper, ruin_value = ruin_value)
    freq = p_list[match(freq, freq_list_drawdown)]
    
    if(!is.null(points)){
      points = unlist(points)
      series = list(sort(unique(c(unlist(series), ((freq * points)) + 1))))
      points = lapply(points, round_2d)
    }
    if(!is.null(series)){
      dataframe = dataframe[unlist(series), ]
    }
    dataframe$years = lapply(dataframe$years, round, digits = 2)
    
    colnames(dataframe) = c("Years", "Mean Withdrawals", "Mean Fund Value", "Probability of Ruin", paste(toOrdinal(100*lower), 'Percentile'), "Median", paste(toOrdinal(100*upper), 'Percentile'))
    table <- datatable(dataframe, options = list(paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
    table <- formatCurrency(table, columns = c("Mean Withdrawals", "Mean Fund Value", paste(toOrdinal(100*lower), 'Percentile'), "Median", paste(toOrdinal(100*upper), 'Percentile')), currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    table <- formatStyle(table, c(1, 4), `border-right` = "solid 1px")
    if(!is.null(points) && !is.null(colour)){
      for(i in 1:length(points)){
        table <- formatStyle(table, "Years", target = 'row', backgroundColor = styleEqual(points[i], colour[i]))
      }
    }
    return(table)
  },

  Drawdown_Plot_Sims <- function(Drawdown_Paths, freq, n_sims = 25, points = NULL, colour = NULL){
    freq = p_list[match(freq, freq_list_drawdown)]
    dat <- vector("list", n_sims)
    p <- ggplot()
    for (i in seq(n_sims)){
      dat[[i]] <- data.frame(years = c(seq(0, ((length(Drawdown_Paths[1, ]) - 1) / freq), by = 1/freq)), capital = Drawdown_Paths[i, ])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = years, y = capital), col = i)
    }
    if (!is.null(points) && !is.null(colour)){
      points = unlist(points)
      for (i in 1:length(points)){
        p <- p + geom_vline(xintercept = points[i], color = colour[i], linetype = 'longdash', width = 3)
      }
    }
    p <- p + 
      labs(x = "Years", y = "Fund Value") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(labels = scales::dollar_format(prefix = "€"), expand = c(0, 0)) +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            # panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    return(p)
  },

  Drawdown_Plot_Percentile <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, lower = 0.05, upper = 0.95, ruin_value = 0, points = NULL, colour = NULL){
    dataframe = Drawdown_DataFrame(Drawdown_Paths = Drawdown_Paths, Drawdown_Withdrawals = Drawdown_Withdrawals, freq = freq, lower = lower, upper = upper, ruin_value = ruin_value)
    freq = p_list[match(freq, freq_list_drawdown)]
    fig <- plot_ly(dataframe, x = ~round(dataframe$years, 2), y = ~round(dataframe$median), name = "Median", type = "scatter", mode = "lines", line = list(color = '#00A7FA', width = 4))
    fig <- fig %>% add_trace(y = ~round(dataframe$lower_bound), name = paste(toOrdinal(100*lower), 'Percentile'), line = list(color = '#FF3B28', width = 4))
    fig <- fig %>% add_trace(y = ~round(dataframe$upper_bound), name = paste(toOrdinal(100*upper), 'Percentile'), line = list(color = '#48EB12', width = 4))
    if (!is.null(points) && !is.null(colour)){
      points = unlist(points)
      colour = unlist(colour)
      vline_list = list()
      for (i in 1:length(points)){
        vline_list[[i]] = plotly_vline(x = points[i], colour = colour[i])
      }
      fig <- fig %>% layout(shapes = vline_list)
    }
    fig <- fig %>% layout(xaxis = list(title = "Years"), yaxis = list(title = list(text = "Fund Value", standoff = 15), tickprefix = '€', tickformat = ",.", ticklabelposition = "inside"))  
    fig <- fig %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.05))
    fig <- fig %>% layout(hovermode = "x unified")
  },

# Partial Drawdown Functions ----------------------------------------------
  Partial_Drawdown_DataFrame <- function(age_1, age_2, freq, annuity_payment, Drawdown_Paths, Drawdown_Withdrawals, buy_later_mean_annuity_payment, BuyLater_Paths, BuyLater_Withdrawals, Deferred_Paths, Deferred_Withdrawals, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    
    annuity_total_paid = numeric(length(Drawdown_Paths[1, ]))
    drawdown_mean_withdrawn = numeric(length(Drawdown_Paths[1, ]))
    drawdown_mean_fund = numeric(length(Drawdown_Paths[1, ]))
    drawdown_median_withdrawn = numeric(length(Drawdown_Paths[1, ]))
    drawdown_median_fund = numeric(length(Drawdown_Paths[1, ]))
    drawdown_prob_ruin = numeric(length(Drawdown_Paths[1, ]))
    buy_later_mean_withdrawn = numeric(length(Drawdown_Paths[1, ]))
    buy_later_mean_fund = numeric(length(Drawdown_Paths[1, ]))
    buy_later_median_withdrawn = numeric(length(Drawdown_Paths[1, ]))
    buy_later_median_fund = numeric(length(Drawdown_Paths[1, ]))
    buy_later_prob_ruin = numeric(length(Drawdown_Paths[1, ]))
    buy_later_total_paid_annuity = numeric(length(Drawdown_Paths[1, ]))
    deferred_mean_withdrawn = numeric(length(Drawdown_Paths[1, ]))
    deferred_mean_fund = numeric(length(Drawdown_Paths[1, ]))
    deferred_median_withdrawn = numeric(length(Drawdown_Paths[1, ]))
    deferred_median_fund = numeric(length(Drawdown_Paths[1, ]))
    deferred_prob_ruin = numeric(length(Drawdown_Paths[1, ]))
    deferred_total_paid_annuity = numeric(length(Drawdown_Paths[1, ]))
    years = c(seq(0, ((length(Drawdown_Paths[1, ]) - 1) / freq), by = 1/freq))
    
    for(i in 1:length(Drawdown_Paths[1, ])){
      annuity_total_paid[i] = i * annuity_payment
      drawdown_mean_withdrawn[i] = Drawdown_Mean_Withdrawals(Drawdown_Withdrawals, i)
      drawdown_mean_fund[i] = Drawdown_Mean(Drawdown_Paths, i)
      drawdown_median_withdrawn[i] = Drawdown_Percentile_Withdrawals(Drawdown_Withdrawals, i)
      drawdown_median_fund[i] = Drawdown_Percentile(Drawdown_Paths, i)
      drawdown_prob_ruin[i] = Drawdown_Ruin_Prob(Drawdown_Paths, i)
      
      if(i <= (freq * (age_2 - age_1) + 1)){
        buy_later_mean_withdrawn[i] = Drawdown_Mean_Withdrawals(BuyLater_Withdrawals, i)
        buy_later_mean_fund[i] = Drawdown_Mean(BuyLater_Paths, i)
        buy_later_median_withdrawn[i] = Drawdown_Percentile_Withdrawals(BuyLater_Withdrawals, i)
        buy_later_median_fund[i] = Drawdown_Percentile(BuyLater_Paths, i)
        buy_later_prob_ruin[i] = Drawdown_Ruin_Prob(BuyLater_Paths, i)
        deferred_mean_withdrawn[i] = Drawdown_Mean_Withdrawals(Deferred_Withdrawals, i)
        deferred_mean_fund[i] = Drawdown_Mean(Deferred_Paths, i)
        deferred_median_withdrawn[i] = Drawdown_Percentile_Withdrawals(Deferred_Withdrawals, i)
        deferred_median_fund[i] = Drawdown_Percentile(Deferred_Paths, i)
        deferred_prob_ruin[i] = Drawdown_Ruin_Prob(Deferred_Paths, i)
        
      } else if (i == (freq * (age_2 - age_1)) + 2){
        buy_later_total_paid_annuity[i] = buy_later_mean_annuity_payment
        buy_later_mean_withdrawn[i] = buy_later_mean_withdrawn[i - 1]
        buy_later_median_withdrawn[i] = buy_later_median_withdrawn[i - 1]
        deferred_total_paid_annuity[i] = annuity_payment
        deferred_mean_withdrawn[i] = deferred_mean_withdrawn[i - 1] + deferred_mean_fund[i - 1]
        deferred_median_withdrawn[i] = deferred_median_withdrawn[i - 1] + deferred_median_fund[i - 1]
        
      } else {
        buy_later_total_paid_annuity[i] = (i - (freq * (age_2 - age_1 + 1))) * buy_later_mean_annuity_payment
        buy_later_mean_withdrawn[i] = buy_later_mean_withdrawn[i - 1]
        buy_later_median_withdrawn[i] = buy_later_median_withdrawn[i - 1]
        deferred_total_paid_annuity[i] = (i - (freq * (age_2 - age_1 + 1))) * annuity_payment
        deferred_mean_withdrawn[i] = deferred_mean_withdrawn[i - 1]
        deferred_median_withdrawn[i] = deferred_median_withdrawn[i - 1]
      }
    }
    buy_later_total = buy_later_total_paid_annuity + buy_later_mean_withdrawn
    deferred_total = deferred_total_paid_annuity + deferred_mean_withdrawn
    
    table_df = data.frame(years, annuity_total_paid,
                          drawdown_mean_withdrawn, drawdown_mean_fund, drawdown_prob_ruin,
                          buy_later_total, buy_later_mean_fund,
                          deferred_total, deferred_mean_fund, deferred_prob_ruin)
    
    return(table_df)
  },

  Partial_Drawdown_Comparison_Table <- function(age_1, age_2, freq, annuity_payment, Drawdown_Paths, Drawdown_Withdrawals, buy_later_mean_annuity_payment, BuyLater_Paths, BuyLater_Withdrawals, Deferred_Paths, Deferred_Withdrawals, series = NULL, points = NULL, colour = NULL, lifetable = ILT15_female_reduced){
    table_df = Partial_Drawdown_DataFrame(age_1 = age_1, age_2 = age_2, freq = freq, annuity_payment = annuity_payment, Drawdown_Paths = Drawdown_Paths, Drawdown_Withdrawals = Drawdown_Withdrawals, buy_later_mean_annuity_payment = buy_later_mean_annuity_payment, BuyLater_Paths = BuyLater_Paths, BuyLater_Withdrawals = BuyLater_Withdrawals, Deferred_Paths = Deferred_Paths, Deferred_Withdrawals = Deferred_Withdrawals, lifetable = lifetable)
    freq = p_list[match(freq, freq_list_drawdown)]
    if(!is.null(points)){
      points = unlist(points)
      series = list(sort(unique(c(unlist(series), ((freq * points)) + 1))))
      points = lapply(points, round_2d)
    }
    if(!is.null(series)){
      table_df = table_df[unlist(series), ]
    }
    table_df$years = lapply(table_df$years, round, digits = 2)
    container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Years', style = "text-align:center; border-right: solid 1px"),
          th(colspan = 1, '100% Annuity', style = "text-align:center; border-right: solid 1px"),
          th(colspan = 3, '100% Drawdown', style = "text-align:center; border-right: solid 1px"),
          th(colspan = 2, 'Drawdown and Annuity Purchase', style = "text-align:center; border-right: solid 1px"),
          th(colspan = 3, 'Drawdown and Deferred Annuity', style = "text-align:center")
        ),
        tr(
          th('Total Received', style = "border-right: solid 1px;"),
          th('Mean Withdrawals'), th('Mean Fund'), th('Prob of Ruin', style = "border-right: solid 1px;"),
          th('Mean Withdrawals / Total Received'), th('Mean Fund', style = "border-right: solid 1px;"),
          th('Mean Withdrawals / Total Received'), th('Mean Fund'), th('Prob of Ruin')
        )
      )
    ))
    table <- datatable(table_df, container = container, options = list(scrollX = TRUE, info = FALSE, paging = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE) %>%
      formatCurrency(columns = c(2, 3, 4, 6, 7, 8, 9), currency = "€") %>%
      formatPercentage(columns = c(5, 10), digits = 2) %>%
      formatStyle(c(1, 2, 5, 7), `border-right` = "solid 1px")
    if(!is.null(points)){
      for(i in 1:length(points)){
        table <- formatStyle(table, 'years', target = 'row', backgroundColor = styleEqual(points[i], colour[i]))
      }
    }
    return(table)
  },

# Risk Profiler Functions -------------------------------------------------
  save_results <- function(session, submit, survey){
    if(submit %% (num.quest + 2) > 0 && (submit %% (num.quest + 2) <= num.quest)){
      try(results[submit %% (num.quest + 2)] <<- survey, silent = T)
    }
    return("")
  },

  portfolio_update <- function(session, input_mean_return, new_mean_return, input_ret_std_dev, new_ret_std_dev){
    updateNumericInputIcon(session, input_mean_return, value = new_mean_return)
    updateNumericInputIcon(session, input_ret_std_dev, value = new_ret_std_dev)
  },

  portfolio <- function(session, input_mean_return, input_ret_std_dev, means = c(2.5, 3, 3.5), std_devs = c(2, 4, 7)){
    if(is.character(results)){
      results <<- parse_vector(results, col_integer()) 
    }
    x = mean(results)
    if(x < 7/3){
      portfolio_update(session, input_mean_return, means[1], input_ret_std_dev, std_devs[1])
      h1("Low Risk Appetite")
    } else if (x <= 11/3){
      portfolio_update(session, input_mean_return, means[2], input_ret_std_dev, std_devs[2])
      h1("Moderate Risk Appetite")
    } else{
      portfolio_update(session, input_mean_return, means[3], input_ret_std_dev, std_devs[3])
      h1("High Risk Appetite")
    }
  },

  riskprofilerui <- function(session, surveydisplay, submit, page, input_mean_return, input_ret_std_dev, mainui, means = c(2.5, 3, 3.5), std_devs = c(2, 4, 7)){
    if(surveydisplay %% 2 == 1 && surveydisplay != 0){
      if(submit %% (num.quest + 2) == 0){
        return(list(
          column(12,
                 img(src='stockmarket.jpeg', height="200px", width="100%")
          ),
          br(),
          br(),
          box(status = "primary", width = 12, solidHeader = F,
              h5("The following questions are intended to assess your capacity for loss and help you to choose \n
                      a suitable portfolio and level of risk."),
              h5("Click next to begin the survey."))
        ))
      } else if ((submit %% (num.quest + 2) > 0) && (submit %% (num.quest + 2) <= num.quest)){
        return(list(
          box(status = "warning", width = 12, solidHeader = T,
              h5("Disclaimer: This risk profiler tool is for illustrative purposes only.
                     It is designed to assist you in understanding your attitude towards risk.")
          ),
          box(status = "primary", width = 12, solidHeader = T,
              h5(paste0(
                "Q", submit %% (num.quest + 2), ": ",
                Qlist[submit %% (num.quest + 2), 2]
              )),
              radioButtons(paste0(page,"_survey"), "Please Select:", myLists[[submit %% (num.quest + 2)]])
          )
        ))
      } else {
        return(list(
          box(status = "warning", width = 12, solidHeader = T,
              h5("Disclaimer: This risk profiler tool is for illustrative purposes only.
                     It is designed to assist you in understanding your attitude towards risk.")
          ),
          box(status = "primary", width = 12, solidHeader = T,
              h5(portfolio(session, input_mean_return, input_ret_std_dev, means, std_devs)),
              hr(),
              h4("Suitable changes have been applied to the mean return and standard deviation of returns."),
              h4("These changes are intended to reflect your risk appetite, but may be changed at any time."),
              hr(),
              h4("Click the risk profiler button (in the sidebar) to return to the summary page."),
              h4("Click next to repeat the survey.")
          )
        ))
      }
    } else {
      return(mainui)
    }
  }
)
