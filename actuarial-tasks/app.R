library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(readxl)

library(lifecontingencies)
options(scipen=999)

# ILT15 Life Tables -------------------------------------------------------
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

# Frequencies -------------------------------------------------------------
freq_list = c("Annually", "Semi-Annually", "Quarterly", "Bi-Monthly", "Monthly", "Fortnightly", "Weekly", "Daily")
p_list = c(1, 2, 4, 6, 12, 26, 52, 365)


# General UI --------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Actuarial Tasks in R"),

    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Loan Calculator", tabName = "loan_calc"),
            menuItem("SORP Calculator", tabName = "sorp"),
            menuItem("Drawdown Simulator", tabName = "drawdown"),
            # menuItem("SORP & Drawdown", tabName = "sorp_x_drawdown"),
            menuItem("SORP & Drawdown", tabName = "sorp_bh")
        )
    ),

    dashboardBody(
        useShinyjs(),
        fluidRow(
            tabItems(
                tabItem(tabName = 'loan_calc', source("source_scripts/loan_calc_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp', source("source_scripts/sorp_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'drawdown', source("source_scripts/drawdown_ui.R", local = TRUE)[1]),
                # tabItem(tabName = 'sorp_x_drawdown', source("source_scripts/sorp_x_drawdown_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp_bh', source("source_scripts/sorp_bh_ui.R", local = TRUE)[1])
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
    # source("source_scripts/sorp_x_drawdown_server.R", local = TRUE)[1]
    source("source_scripts/sorp_bh_server.R", local = TRUE)[1]
}

# Run the application 
shinyApp(ui = ui, server = server)