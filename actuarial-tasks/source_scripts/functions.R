list(
  ILT15_female_reduced_widowed <- function(relationship = 1, widowed = NA){
    cnames <- read_excel("data/ILT15.xlsx", sheet = 1, n_max = 0) %>%
      names()
    
    life_table_female <- read_xlsx("data/ILT15.xlsx", sheet = 1, skip=1, col_names = cnames) %>% 
      drop_na()
    
    qx_female <- unlist(life_table_female[,5] * 0.5)
    
    if(relationship == 3){
        percent_diff_bh = c(4.472805, 0.5530933, 0.1864749, 0.6302159, 0.7439571)
        for(i in widowed:(widowed + 4)){
          qx_female[i] = qx_female[i] * (1+percent_diff_bh[i+1-widowed])
      }
    }
    
    ILT15_female_reduced <- probs2lifetable(probs=qx_female,radix=100000,"qx",name="ILT15_female_reduced")
    return(ILT15_female_reduced)
  },
  
  SORP <- function(age_1, age_2, relationship, sal, fundvalue, PreK, PostK, emp_contri, empr_contri, salEsc, iPost, annEsc, guaranteed, equity, fixed, cash, investCharge, equity_p = 4.5, fixed_p = 1, cash_p = 0){
    
    preK = p_list[match(PreK, freq_list)]
    postK = p_list[match(PostK, freq_list)]
    
    iPre = ((equity/100) * (equity_p/100)) + ((fixed/100) * (fixed_p/100)) + ((cash/100) * (cash_p/100)) - (investCharge/100)
    netiPost = ((1 + (iPost/100))/(1 + (annEsc/100))) - 1
    
    iPreK = effective2Convertible(i=iPre, k=preK)
    iPostK = effective2Convertible(i=(iPost/100), k=postK)
    
    fundValueX = numeric((age_2 - age_1)*preK + 2)
    ages = numeric((age_2 - age_1)*preK + 2)
    ages_exact = numeric((age_2 - age_1)*preK + 2)
    periods = numeric((age_2 - age_1)*preK + 2)
    EEContribution = numeric((age_2 - age_1)*preK + 2)
    ERContribution = numeric((age_2 - age_1)*preK + 2)
    totalContribution = numeric((age_2 - age_1)*preK + 2)
    sorp_vector = numeric((age_2 - age_1)*preK + 2)
    
    #annuities will increase by 0.33% per annum compound
    retire_year = as.numeric(format(Sys.Date(), "%Y")) + (age_2 - age_1)
    ann_inc_rate = 1.0033^(retire_year - 2013)
    
    ages[1] = age_1 - 1
    ages_exact[1] = age_1 - 1/preK
    periods[1] = preK - 1
    fundValueX[1] = fundvalue
    
    for (m in 2:((age_2 - age_1)*preK + 2)){
      ages[m] = ages[m-1]
      ages_exact[m] = ages_exact[m - 1] + 1/preK
      periods[m] = periods[m - 1] + 1
      EEContribution[m] = sal*(emp_contri/100)*1/preK
      ERContribution[m] = sal*(empr_contri/100)*1/preK
      totalContribution[m] = EEContribution[m] + ERContribution[m]
      fundValueX[m] = fundValueX[m-1]*(1+iPreK/preK) + totalContribution[m]
      
      if((m - 1)%%preK == 0){
        sal = sal * (1 + (salEsc/100))
      }
      
      if((m - 2)%%preK == 0){
        periods[m] = 0
        ages[m] = ages[m - 1] + 1
      }
    }
    
    guar_ann = annuity(i = netiPost, n = guaranteed, k = postK, type = "advance")
    
    if (relationship == 1) {
      SORP_Annuity = (guar_ann + axn(ILT15_female_reduced, x = age_2, i = netiPost, k = postK, m = guaranteed, payment = "advance"))*ann_inc_rate
    } else {
      SORP_Annuity = (guar_ann + axyzn(listOfTables, x = c(age_2, age_2), i = netiPost, m = guaranteed, k = postK, status = "last", payment = "advance"))*ann_inc_rate
    }
    
    sorp_vector[1] = SORP_Annuity
    
    df_fund <- data.frame(age = ages, period = periods, age_exact = ages_exact,
                          EmployeeContribution = EEContribution,
                          EmployerContribution = ERContribution,
                          TotalContribution = totalContribution,
                          FundValue = fundValueX,
                          SORPAnnuity = sorp_vector
    )
    
    return(df_fund)
  },
  
  Drawdown_Sim <- function(retire_age, start_capital, withdraw_freq, annual_mean_return, annual_ret_std_dev, annual_inflation, annual_inf_std_dev, n_sim, percent_yn = F, annual_withdrawals = 28000, percent_withdrawal = 4, relationship = 1, widowed = NA){
    ILT15_female_reduced = ILT15_female_reduced_widowed(relationship, widowed)
    
    #-------------------------------------
    #Assignment
    #-------------------------------------
    
    p = p_list[match(withdraw_freq, freq_list)]
    start.capital = start_capital
    
    # Investment
    annual.mean.return = annual_mean_return / 100
    annual.ret.std.dev = annual_ret_std_dev / 100
    
    # Inflation
    annual.inflation = annual_inflation / 100
    annual.inf.std.dev = annual_inf_std_dev / 100
    
    # Withdrawals
    if (percent_yn == F) {
      periodic.withdrawals = annual_withdrawals / p
    } else {
      periodic.percent.withdrawal = effective2Convertible(i=percent_withdrawal/100,k=p)/p
    }
    
    # Life Expectancy
    n_years = exn(ILT15_female_reduced, retire_age)
    
    # number of periods to simulate
    n_obs = p * n_years
    
    # periodic Investment and Inflation assumptions
    periodic.mean.return = effective2Convertible(i=annual.mean.return,k=p)/p
    periodic.ret.std.dev = annual.ret.std.dev / sqrt(p)
    
    periodic.inflation = effective2Convertible(i=annual.inflation,k=p)/p
    periodic.inf.std.dev = annual.inf.std.dev / sqrt(p)
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    Spaths = matrix(0, n_sim, n_obs+1)
    periodic_withdrawals = matrix(0, n_sim, n_obs)
    Spaths[,1] = start.capital
    
    periodic.invest.returns = numeric(n_obs)
    periodic.inflation.returns = numeric(n_obs)
    
    for(i in 1:n_sim){
      
      periodic.invest.returns = rnorm(n_obs, mean = periodic.mean.return, sd = periodic.ret.std.dev)
      periodic.inflation.returns = rnorm(n_obs, mean = periodic.inflation, sd = periodic.inf.std.dev)
      if (percent_yn == F) {
        if (Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawals <= 0) {break}
        Spaths[i,2] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1])-periodic.withdrawals
        periodic_withdrawals[i, 1] = periodic.withdrawals
      } else {
        if (Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1]) * (1 - periodic.percent.withdrawal) <= 0) {break}
        Spaths[i,2] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1]) * (1 - periodic.percent.withdrawal)
        periodic_withdrawals[i, 1] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1]) * (periodic.percent.withdrawal)
      }
      
      for(j in 2:n_obs){
        if (percent_yn == F) {
          if (Spaths[i,j] <= 0) {
            break
          } else if (Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawals <= 0) {
            break
          } else {
            Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])-periodic.withdrawals
            periodic_withdrawals[i, j] = periodic.withdrawals
          }
        } else {
          if (Spaths[i,j] <= 0) {
            break
          } else if (Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])*(1 - (periodic.percent.withdrawal * prod(1 + periodic.inflation.returns[1:j-1]))) <= 0) {
            break
          } else {
            Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])*(1 - (periodic.percent.withdrawal * prod(1 + periodic.inflation.returns[1:j-1])))
            periodic_withdrawals[i, j] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j])*(periodic.percent.withdrawal * prod(1 + periodic.inflation.returns[1:j-1]))
          }
        }
      }
    }
    return(Spaths)
  }
)