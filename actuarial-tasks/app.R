library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(readxl)
library(writexl)
library(janitor)
library(plyr)
library(tidyquant)
library(lubridate)
library(toOrdinal)
library(scales)
library(xkcd)
library(shinyBS)
library(lifecontingencies)
options(scipen=999)

# ILT15 Life Tables -------------------------------------------------------
cnames <- read_excel("data/ILT15.xlsx", sheet = 1, n_max = 0) %>%
    names()

life_table_female <- read_xlsx("data/ILT15.xlsx", sheet = 1, skip = 1, col_names = cnames) %>% 
    drop_na()

life_table_male <- read_xlsx("data/ILT15.xlsx", sheet = 2, skip=1, col_names = cnames) %>% 
    drop_na()

qx_female <- unlist(life_table_female[,5] * 0.5)
qx_male <- unlist(life_table_male[,5] * 0.42)

ILT15_female_reduced <- probs2lifetable(probs = qx_female, radix = 100000, type = "qx", name = "ILT15_female_reduced")
ILT15_male_reduced <- probs2lifetable(probs = qx_male, radix = 100000, type = "qx", name = "ILT15_male_reduced")
listOfTables <- list(ILT15_female_reduced, ILT15_male_reduced)

# Broken Heart Life Table -------------------------------------------------
BrokenHeart_LifeTable <- function(widowed_status = FALSE, widowed_age = NULL, gender = 1){
    cnames <- read_excel("data/Broken Heart Life Table.xlsx", sheet = 1, n_max = 0) %>%
        names()
    
    if(gender == 1){
        life_table <- read_xlsx("data/Broken Heart Life Table.xlsx", sheet = 2, skip = 1, col_names = cnames) %>% 
            drop_na()
        band_1 = c(0.0295, 0.0321, 0.0223, 0.0163, 0.0226)
        band_2 = c(0.1221, 0.0146, 0.0653, 0.0351, 0.0000)
        band_3 = c(0.1954, 0.1713, 0.0688, 0.0806, 0.0000)
    } else {
        life_table <- read_xlsx("data/Broken Heart Life Table.xlsx", sheet = 1, skip = 1, col_names = cnames) %>% 
            drop_na()
        band_1 = c(0.1835, 0.0695, 0.0254, 0.0556, 0.0000)
        band_2 = c(0.4699, 0.0923, 0.0461, 0.0000, 0.2322)
        band_3 = c(0.5097, 0.1452, 0.0888, 0.2150, 0.3785)
    }
    
    qx <- unlist(life_table[,2])
    
    if(widowed_status == TRUE){
        for(i in widowed_age:(widowed_age + 4))
            if(i <= 75){
                qx[i + 1] = band_1[i + 1 - widowed_age]
            } else if (76 <= i && i <= 85){
                qx[i + 1] = band_2[i + 1 - widowed_age]
            } else {
                qx[i + 1] = band_3[i + 1 - widowed_age]
            }
    }
    
    broken_heart_lifetable <- probs2lifetable(probs = qx, radix = 11454, "qx", name = "broken_heart_lifetable")
    return(broken_heart_lifetable)
}

# Risk Profiler - Question List Import ------------------------------------
Qlist <- read.csv("data/Qlist.csv")
num.quest = nrow(Qlist)
results <- numeric(num.quest)
myLists = vector("list", nrow(Qlist))
for(i in(1:nrow(Qlist))){
    myListX = list()
    for(j in (1:(ncol(Qlist)-2))){
        myListX[Qlist[i,j+2]] = ncol(Qlist) - 1 - j
    }
    myLists[[i]] = myListX
}


# Historical Data Preamble ------------------------------------------------
## Load data
# Create symbol vectors
symbols <- c("WILL5000INDFC", "BAMLCC0A0CMTRIV", "GOLDPMGBD228NLBM", "CSUSHPINSA", "DGS5", "IRLCPIALLMINMEI")
sym_names <- c("stock", "bond", "gold", "realt", "rfr")

# Get symbols
getSymbols(symbols, src="FRED", from = "1970-01-01", to = "2020-12-31")

# Merge xts objects and resample to monthly
index <- merge(WILL5000INDFC, BAMLCC0A0CMTRIV, GOLDPMGBD228NLBM, CSUSHPINSA, DGS5)
index <- na.locf(index)
colnames(index) <- sym_names

inflation <- IRLCPIALLMINMEI
inflation <- na.locf(inflation)
colnames(inflation)[1] = "inflation"

idx_mon <- to.monthly(index, indexAt = "lastof", OHLC=FALSE)
idx_mon <- idx_mon["1987/2020"]

inf_mon <- to.monthly(inflation, indexAt = "lastof", OHLC = FALSE)
inf_mon <- inf_mon["1987/2020"]

# Create data frame
index_df <- data.frame(date = index(idx_mon), coredata(idx_mon)) %>%
  mutate_at(vars(-c(date, rfr)), function(x) x/lag(x)-1) %>%
  mutate(rfr = effective2Convertible(i=rfr/100,k=60)/60)

index_df_inflation <- data.frame(date = index(inf_mon), coredata((inf_mon)))
for(i in nrow(index_df_inflation):2){
  index_df_inflation[i,2] = (index_df_inflation[i,2] - index_df_inflation[i-1,2])/index_df_inflation[i-1,2]
}

portfolio_list = c("Stocks", "Bonds", "Gold", "Real Estate", "Risk Free Rate")

# Frequencies -------------------------------------------------------------
freq_list = c("Annually", "Semi-Annually", "Quarterly", "Bi-Monthly", "Monthly", "Fortnightly", "Weekly", "Daily")
freq_list_drawdown = c("Annually", "Semi-Annually", "Quarterly (Slow)", "Bi-Monthly (Very Slow)", "Monthly (Extremely Slow)")
p_list = c(1, 2, 4, 6, 12, 26, 52, 365)

# Rounding to 2 Decimal Places --------------------------------------------
round_2d <- function(x, two_d = F){
    if(two_d == F) {
        if(round(as.numeric(x), 1)%%1 == 0){
          return(format(round(as.numeric(x), 0), nsmall = 0, big.mark = ",", scientific=FALSE))
        } else if ((10*round(as.numeric(x), 2))%%1 == 0){
          return(format(round(as.numeric(x), 1), nsmall = 1, big.mark = ",", scientific=FALSE))
        } 
    }
    return(format(round(as.numeric(x), 2), nsmall = 2, big.mark = ",", scientific=FALSE))
}

# Plotly - Horizontal & Vertical Lines ------------------------------------
plotly_hline <- function(y = 0, colour = "blue") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = colour, width = 3, dash = 'dash'))
}

plotly_vline <- function(x = 0, colour = "red") {
  list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = x, x1 = x, line = list(color = colour, width = 3, dash = 'dash'))
}

# General UI --------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Actuarial Tasks in R",
                    dropdownMenu(
                      type = "notifications", 
                      headerText = strong("See the code behind the app:"), 
                      icon = icon("code"), 
                      badgeStatus = NULL,
                      notificationItem(
                        text = "Click here to visit the GitHub Page",
                        href = "https://github.com/Jimzo123/FYP_MS4090",
                        icon = icon("github")
                        )
                      )
                    ),

    dashboardSidebar(
        
        sidebarMenu(
          menuItem("Loan Calculator", tabName = "loan_calc", icon = icon("landmark")),
          menuItem("SORP Calculator", tabName = "sorp", icon = icon("calculator")),
          menuItem("Drawdown Simulator", tabName = "drawdown", icon = icon("chart-line")),
          menuItem("SORP & Drawdown", tabName = "sorp_x_drawdown", icon = icon("dashboard")),
          menuItem("Drawdown Strategies", tabName = "partial_drawdown", icon = icon("tags")),
          menuItem("Broken Heart Effect", tabName = "broken_heart", icon = icon("heart-broken")),
          menuItem("SORP Import", tabName = "sorp_import", icon = icon("file-import")),
          menuItem("Life Expectancy Visualisations", tabName = "life_ex", icon = icon("heartbeat")),
          menuItem("Sequencing Risk", tabName = "sequencing", icon = icon("sync")),
          menuItem("Historical Simulation", tabName = "hist_data", icon = icon("history"))
          
        )
    ),

    dashboardBody(
        tags$style(".well {background-color:white; border-color:#3C8DBC; margin-left:1em;}
                   .nav-tabs-custom {border: 1px solid #3C8DBC}
                   .box {border: 1px solid #3C8DBC}"),
        useShinyjs(),
        fluidRow(
            tabItems(
                tabItem(tabName = 'loan_calc', source("source_scripts/loan_calc_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp', source("source_scripts/sorp_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'drawdown', source("source_scripts/drawdown_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp_x_drawdown', source("source_scripts/sorp_x_drawdown_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'partial_drawdown', source("source_scripts/partial_drawdown_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'broken_heart', source("source_scripts/broken_heart_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp_import', source("source_scripts/sorp_import_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'life_ex', source("source_scripts/life_ex_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sequencing', source("source_scripts/sequencing_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'hist_data', source("source_scripts/historical_data_ui.R", local = TRUE)[1])
                
            )
        )
    )
)

# General Server ----------------------------------------------------------
server <- function(input, output, session) {
    source("source_scripts/functions.R", local = TRUE)[1]
    source("source_scripts/loan_calc_server.R", local = TRUE)[1]
    source("source_scripts/sorp_server.R", local = TRUE)[1]
    source("source_scripts/drawdown_server.R", local = TRUE)[1]
    source("source_scripts/sorp_x_drawdown_server.R", local = TRUE)[1]
    source("source_scripts/partial_drawdown_server.R", local = TRUE)[1]
    source("source_scripts/broken_heart_server.R", local = TRUE)[1]
    source("source_scripts/sorp_import_server.R", local = TRUE)[1]
    source("source_scripts/life_ex_server.R", local = TRUE)[1]
    source("source_scripts/sequencing_server.R", local = TRUE)[1]
    source("source_scripts/historical_data_server.R", local = TRUE)[1]
}

# Run the application 
shinyApp(ui = ui, server = server)