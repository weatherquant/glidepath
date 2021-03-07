library("CASdatasets")
attach(data("canlifins"))
library("lubridate")

start_date = as.Date("1988-12-29")
end_date = as.Date("1993-12-31")
duration = as.numeric(end_date - start_date)

canlifins_days = canlifins
as.numeric(as.Date(round_date(date_decimal(decimal_date(start_date) + canlifins[14, 4]), "day")) - start_date)


for(i in 1:length(canlifins$EntryAgeM)){
  canlifins_days[i, 3] = as.numeric(as.Date(round_date(date_decimal(decimal_date(start_date) + canlifins_days[i, 3]), "day")) - start_date)
  canlifins_days[i, 4] = as.numeric(as.Date(round_date(date_decimal(decimal_date(start_date) + canlifins_days[i, 4]), "day")) - start_date)
}

# Zero Death
canlifins_zero = canlifins_days
for(i in 1:length(canlifins_zero$EntryAgeM)){
  if((canlifins_zero[i, 3] != 0 | canlifins_zero[i, 4] != 0)){
    canlifins_zero[i, ] = NA
  }
}
canlifins_zero = na.omit(canlifins_zero)

# One Death
canlifins_one = canlifins_days
for(i in 1:length(canlifins_one$EntryAgeM)){
  if((canlifins_one[i, 3] == 0 && canlifins_one[i, 4] == 0) | (canlifins_one[i, 3] != 0 && canlifins_one[i, 4] != 0)){
    canlifins_one[i, ] = NA
  }
}
canlifins_one = na.omit(canlifins_one)

# Two Deaths
canlifins_two = canlifins_days
for(i in 1:length(canlifins_two$EntryAgeM)){
  if(canlifins_two[i, 3] == 0 | canlifins_two[i, 4] == 0){
    canlifins_two[i, ] = NA
  }
}
canlifins_two = na.omit(canlifins_two)

for(i in 1:length(canlifins_two$EntryAgeM)){
  canlifins_two[i, 6] = abs(canlifins_two[i, 3] - canlifins_two[i, 4])
}
colnames(canlifins_two)[6] = "WidowhoodLength"
hist(canlifins_two$WidowhoodLength)

total_widows = (length(canlifins_one$EntryAgeM) + length(canlifins_two$EntryAgeM))

total_lives = length(canlifins$EntryAgeM)*2
total_deaths = length(canlifins_one$EntryAgeM) + length(canlifins_two$EntryAgeM)*2

prob_die = total_deaths / total_lives
prob_die_day = prob_die/duration

total_bh_i = prob_bh_i = prob_die_v_i = diff_prob_i = numeric(duration)
for(i in 1:duration){
  total_bh_i[i] = length(which(canlifins_two$WidowhoodLength == i))
  prob_bh_i[i] = total_bh_i[i]/total_widows
  prob_die_v_i[i] = prob_die_day
  # diff_prob_i[i] = abs(prob_bh_i[i] - prob_die_v_i[i])
}
bh_i = data.frame(total_bh_i, prob_bh_i, prob_die_v_i)
bh_i[c(365, 2*365, 3*365, 4*365, 5*365), ]

total_bh_c = prob_bh_c = prob_die_v_c = diff_prob_c = numeric(duration)
for(i in 1:duration){
  total_bh_c[i] = length(which(canlifins_two$WidowhoodLength <= i))
  prob_bh_c[i] = total_bh_c[i]/total_widows
  prob_die_v_c[i] = (prob_die * i) / duration
  diff_prob_c[i] = abs(prob_bh_c[i] - prob_die_v_c[i])
}
bh_c = data.frame(total_bh_c, prob_bh_c, prob_die_v_c, diff_prob_c)
bh_c[c(365, 2*365, 3*365, 4*365, 5*365), ]

y1_c = apply(bh_i[1:365, ], 2, sum) # Year 1 Cumulative
y2_c = apply(bh_i[366:730, ], 2, sum) # Year 2 Cumulative
y3_c = apply(bh_i[731:1095, ], 2, sum) # Year 3 Cumulative
y4_c = apply(bh_i[1096:1460, ], 2, sum) # Year 4 Cumulative
y5_c = apply(bh_i[1460:duration, ], 2, sum) # Year 5 Cumulative
y_c = data.frame(y1_c, y2_c, y3_c, y4_c, y5_c)
diff_prob = numeric(5)
for(i in 1:5){
  diff_prob[i] = y_c[2, i] - y_c[3, i]
}
y_c
diff_prob
percent_diff_bh = diff_prob/y_c[3, ]

# broken_heart_6m = length(which(canlifins_two$WidowhoodLength < 183)) #122 broken heart within 6 months
# broken_heart_6m/total_widows #prob of broken heart given one death
# 
# broken_heart_1d = length(which(canlifins_two$WidowhoodLength <= 1))
# broken_heart_1d/total_widows
# 
# broken_heart_1m = length(which(canlifins_two$WidowhoodLength <= 30))
# prob_bh_1d = broken_heart_1m/total_widows
# 
# broken_heart_2m = length(which(canlifins_two$WidowhoodLength <= 61))
# broken_heart_2m/total_widows

# ILT15 Life Tables -------------------------------------------------------
library(DT)
library(readxl)
library(lifecontingencies)
setwd("/Users/tommycornally/Documents/Actuarial Tasks in R/actuarial-tasks")
cnames <- read_excel("data/ILT15.xlsx", sheet = 1, n_max = 0) %>%
  names()

life_table_female <- read_xlsx("data/ILT15.xlsx", sheet = 1, skip=1, col_names = cnames) %>% 
  drop_na()

life_table_male <- read_xlsx("data/ILT15.xlsx", sheet = 2, skip=1, col_names = cnames) %>% 
  drop_na()

qx_female <- unlist(life_table_female[,5] * 0.5)
qx_male <- unlist(life_table_male[,5] * 0.42)

ILT15_female_reduced <- probs2lifetable(probs=qx_female,radix=100000,"qx",name="ILT15_female_reduced")
ILT15_male_reduced <- probs2lifetable(probs=qx_male,radix=100000,"qx",name="ILT15_male_reduced")
listOfTables <- list(ILT15_female_reduced, ILT15_male_reduced)

age_widowed = 66
for(i in age_widowed:(age_widowed + 4)){
  qx_female[i] = qx_female[i] * (1+percent_diff_bh[,i+1-age_widowed])
}

ILT15_female_reduced <- probs2lifetable(probs=qx_female,radix=100000,"qx",name="ILT15_female_reduced")
ILT15_male_reduced <- probs2lifetable(probs=qx_male,radix=100000,"qx",name="ILT15_male_reduced")
listOfTables <- list(ILT15_female_reduced, ILT15_male_reduced)

# if(spouse dead){
#   for(i in age at spouse death:that + 5)
#     qx_female[i] = qx_female[i] * * (1+percent_diff_bh[i])
# }

