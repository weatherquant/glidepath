list(
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
    
    fundValueX = numeric((age_2 - age_1)*freq + 2)
    ages = numeric((age_2 - age_1)*freq + 2)
    ages_exact = numeric((age_2 - age_1)*freq + 2)
    periods = numeric((age_2 - age_1)*freq + 2)
    EEContribution = numeric((age_2 - age_1)*freq + 2)
    ERContribution = numeric((age_2 - age_1)*freq + 2)
    totalContribution = numeric((age_2 - age_1)*freq + 2)
    interestOnFund = numeric((age_2 - age_1)*freq + 2)
    
    ages[1] = age_1 - 1
    ages_exact[1] = age_1 - 1/freq
    periods[1] = freq
    fundValueX[1] = current_fundvalue
    
    for (m in 2:((age_2 - age_1)*freq + 2)){
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
  
  SORP_Fund <- function(SORP_Contributions, freq){
    freq = p_list[match(input$sorp_pre_freq, freq_list)]
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

# To Finalize
  SORP_Plot_FundValue <- function(SORP_Contributions){
    AgeandFundValue <- SORP_Contributions %>% select(age_exact, FundValue)
    p <- ggplot(AgeandFundValue, aes(x = age_exact, y = FundValue, fill = "#4A8DBF", colour = "#4A8DBF")) +
      geom_bar(stat = "identity", colour = "#4A8DBF", fill = "#4A8DBF") + 
      labs(x = "Age", y = "Fund Value", fill = NULL, color = NULL) +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    return(p)
  },

# To Finalize
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
    SORP_Contributions <- datatable(SORP_Contributions, options = list(paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames= FALSE)
    SORP_Contributions <- formatCurrency(SORP_Contributions, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period"), currency = "€")
    return(SORP_Contributions)
  },

# Drawdown Functions ------------------------------------------------------
  Drawdown_Simulations <- function(retire_age, start_capital, withdraw_freq, annual_mean_return, annual_ret_std_dev, annual_inflation, annual_inf_std_dev, n_sim = 10000, withdraw_type = F, annual_withdrawals = 15000, percent_withdrawal = 4, life_table = ILT15_female_reduced, end_age = NA){
    
    p = p_list[match(withdraw_freq, freq_list)]
    
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
      periodic_percent_withdrawal = start_capital * effective2Convertible(i = percent_withdrawal / 100, k = p) / p
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
    
    paths_matrix = matrix(0, n_sim, n_obs + 1)
    withdrawals_matrix = matrix(0, n_sim, n_obs + 1)
    paths_matrix[, 1] = start_capital
    
    periodic_invest_returns = numeric(n_obs)
    periodic_inflation_returns = numeric(n_obs)
    
    if (withdraw_type == F){
      for(i in 1:n_sim){
        periodic_invest_returns = rnorm(n_obs, mean = periodic_mean_return, sd = periodic_ret_std_dev)
        periodic_inflation_returns = rnorm(n_obs, mean = periodic_inflation, sd = periodic_inf_std_dev)
      
        next_fund_value = paths_matrix[i, 1] * (1 + periodic_invest_returns[1] - periodic_inflation_returns[1]) - periodic_withdrawals
        if (next_fund_value <= 0) {next}
        paths_matrix[i, 2] = next_fund_value
        withdrawals_matrix[i, 2] = periodic_withdrawals
      
        for(j in 2:n_obs){
          next_fund_value = paths_matrix[i, j] * (1 + periodic_invest_returns[j] - periodic_inflation_returns[j]) - periodic_withdrawals
          if(next_fund_value <= 0){
            break
          } else {
            paths_matrix[i, j + 1] = next_fund_value
            withdrawals_matrix[i, j + 1] = periodic_withdrawals
            }
        }
      }
    } else {
      for(i in 1:n_sim){
        periodic_invest_returns = rnorm(n_obs, mean = periodic_mean_return, sd = periodic_ret_std_dev)
        periodic_inflation_returns = rnorm(n_obs, mean = periodic_inflation, sd = periodic_inf_std_dev)
        
        next_fund_value = paths_matrix[i, 1] * (1 + periodic_invest_returns[1] - periodic_inflation_returns[1]) - periodic_percent_withdrawal
        if (next_fund_value <= 0) {next}
        paths_matrix[i, 2] = next_fund_value
        withdrawals_matrix[i, 2] = periodic_percent_withdrawal
        
        for(j in 2:n_obs){
          next_fund_value = paths_matrix[i, j] * (1 + periodic_invest_returns[j] - periodic_inflation_returns[j]) - (periodic_percent_withdrawal * prod(1 + periodic_inflation_returns[1:j-1]))
          if(next_fund_value <= 0){
            break
          } else {
            paths_matrix[i, j + 1] = next_fund_value
            withdrawals_matrix[i, j + 1] = periodic_percent_withdrawal * prod(1 + periodic_inflation_returns[1:j-1])
          }
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

  Drawdown_Percentile <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ]), percentile = 0.50){
    return(quantile(Drawdown_Paths[, time], probs = percentile))
  },

  Drawdown_Mean_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list)]
    if(is.null(ex)){
      ex = exn(lifetable, age)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Mean(Drawdown_Paths, time))
  },

  Drawdown_Ruin_Prob <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ]), ruin_value = 0){
    return(length(which(Drawdown_Paths[, time] <= ruin_value)) / length(Drawdown_Paths[, time]))
  },

  Drawdown_Ruin_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced, ruin_value = 0){
    freq = p_list[match(freq, freq_list)]
    if(is.null(ex)){
      ex = exn(lifetable, age)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Ruin_Prob(Drawdown_Paths, time, ruin_value))
  },

  Drawdown_Total_Withdrawals <- function(Drawdown_Withdrawals, time = length(Drawdown_Withdrawals[1, ])){
    if(time == 1){
      return(mean(Drawdown_Withdrawals[, time]))
    } else {
      return(mean(rowSums(Drawdown_Withdrawals[, 1:time])))
    }
  },

  Drawdown_Total_Withdrawals_Life_Ex <- function(Drawdown_Withdrawals, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list)]
    if(is.null(ex)){
      ex = exn(lifetable, age)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Total_Withdrawals(Drawdown_Withdrawals, time))
  },

# To Finalize
  Drawdown_DataFrame <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, ruin_value = 0, lower = 0.25, upper = 0.75){
    freq = p_list[match(freq, freq_list)]
  
    total_withdrawn = numeric(length(Drawdown_Withdrawals[1, ]))
    average = numeric(length(Drawdown_Paths[1, ]))
    prob_ruin = numeric(length(Drawdown_Paths[1, ]))
    lower_bound = numeric(length(Drawdown_Paths[1, ]))
    median = numeric(length(Drawdown_Paths[1, ]))
    upper_bound = numeric(length(Drawdown_Paths[1, ]))
    years = c(seq(0, ((length(Drawdown_Paths[1, ]) - 1) / freq), by = 1/freq))
  
    for(i in 1:length(Drawdown_Paths[1, ])){
      total_withdrawn[i] = Drawdown_Total_Withdrawals(Drawdown_Withdrawals, i)
      average[i] = Drawdown_Mean(Drawdown_Paths, i)
      prob_ruin[i] = Drawdown_Ruin_Prob(Drawdown_Paths, i, ruin_value)
      lower_bound[i] = Drawdown_Percentile(Drawdown_Paths, i, percentile = lower)
      median[i] = Drawdown_Percentile(Drawdown_Paths, i)
      upper_bound[i] = Drawdown_Percentile(Drawdown_Paths, i, percentile = upper)
    }
    dataframe = data.frame(years, total_withdrawn, average, prob_ruin, lower_bound, median, upper_bound)
    return(dataframe)
  },

# To Finalize
  Drawdown_Table <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, series = NULL, points = NULL, colour = NULL, ruin_value = 0, lower = 0.25, upper = 0.75){
    dataframe = Drawdown_DataFrame(Drawdown_Paths, Drawdown_Withdrawals, freq, ruin_value, lower, upper)
    if(!is.null(points)){
      series = sort(unique(c(series, points)))
    }
    if(!is.null(series)){
      dataframe = dataframe[series, ]
    }
    colnames(dataframe) = c("Years", "Total Withdrawn", "Average Fund Value", "Probability of Ruin", "Lower Bound", "Median", "Upper Bound")
    table <- datatable(dataframe, options = list(paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
    table <- formatCurrency(table, columns = c("Total Withdrawn", "Average Fund Value", "Lower Bound", "Median", "Upper Bound"), currency = "€")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 2)
    if(!is.null(points)){
      for(i in 1:length(points)){
        table <- formatStyle(table, 'Years', target = 'row', backgroundColor = styleEqual(points[i], colour[i]))
      }
    }
    return(table)
  },

# To Finalize
  Drawdown_Plot_Sims <- function(Drawdown_Paths, n_sims = 25){
      dat <- vector("list", n_sims)
      p <- ggplot()
      for (i in seq(n_sims)){
        dat[[i]] <- data.frame(time = (0:(length(Drawdown_Paths[1, ]) - 1)), capital = Drawdown_Paths[i, ])
        p <- p + geom_line(data = dat[[i]], mapping = aes(x = time, y = capital), col = i)
      }
      return(p)
  }
)