list(
  broken_heart_life_table <- function(status = T, widowed = NA, gender = 1){
    cnames <- read_excel("data/Broken Heart Life Table.xlsx", sheet = 1, n_max = 0) %>%
      names()
    
    if(gender == 1){
        life_table <- read_xlsx("data/Broken Heart Life Table.xlsx", sheet = 2, skip=1, col_names = cnames) %>% 
          drop_na()
        band_1 = c(0.0295, 0.0321, 0.0223, 0.0163, 0.0226)
        band_2 = c(0.1221, 0.0146, 0.0653, 0.0351, 0.0000)
        band_3 = c(0.1954, 0.1713, 0.0688, 0.0806, 0.0000)
    } else {
        life_table <- read_xlsx("data/Broken Heart Life Table.xlsx", sheet = 1, skip=1, col_names = cnames) %>% 
            drop_na()
        band_1 = c(0.1835, 0.0695, 0.0254, 0.0556, 0.0000)
        band_2 = c(0.4699, 0.0923, 0.0461, 0.0000, 0.2322)
        band_3 = c(0.5097, 0.1452, 0.0888, 0.2150, 0.3785)
    }
    
    qx <- unlist(life_table[,2])
    
    if(status == T){
      for(i in widowed:(widowed + 4))
        if(i <= 75){
          qx[i + 1] = band_1[i + 1 - widowed]
        } else if (76 <= i && i <= 85){
          qx[i + 1] = band_2[i + 1 - widowed]
        } else {
          qx[i + 1] = band_3[i + 1 - widowed]
        }
    }
    
    broken_heart_lifetable <- probs2lifetable(probs=qx,radix=11454,"qx",name="broken_heart_lifetable")
    return(broken_heart_lifetable)
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
  
  Drawdown_Sim <- function(retire_age, start_capital, withdraw_freq, annual_mean_return, annual_ret_std_dev, annual_inflation, annual_inf_std_dev, n_sim = 10000, percent_yn = F, annual_withdrawals = 28000, percent_withdrawal = 4, retire_age_spouse = retire_age, life_table = ILT15_female_reduced){
    
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
      periodic.percent.withdrawal = start.capital * effective2Convertible(i=percent_withdrawal/100,k=p)/p
    }
    
    # Time to Run
    n_years = getOmega(life_table) - retire_age
    
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
        if (Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1]) - (periodic.percent.withdrawal) <= 0) {break}
        Spaths[i,2] = Spaths[i,1]*(1+periodic.invest.returns[1]-periodic.inflation.returns[1]) - (periodic.percent.withdrawal)
        periodic_withdrawals[i, 1] = (periodic.percent.withdrawal)
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
          } else if (Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j]) - (periodic.percent.withdrawal * prod(1 + periodic.inflation.returns[1:j-1])) <= 0) {
            break
          } else {
            Spaths[i,j+1] = Spaths[i,j]*(1+periodic.invest.returns[j]-periodic.inflation.returns[j]) - (periodic.percent.withdrawal * prod(1 + periodic.inflation.returns[1:j-1]))
            periodic_withdrawals[i, j] = (periodic.percent.withdrawal * prod(1 + periodic.inflation.returns[1:j-1]))
          }
        }
      }
    }
    return(Spaths)
  },
  
  tommy_round <- function(x){
    return(format(round(as.numeric(x), 2), nsmall = 2, big.mark = ",", scientific=FALSE))
  }
)