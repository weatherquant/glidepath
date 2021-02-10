library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(readxl)

library(lifecontingencies)
options(scipen=999)

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
            menuItem("SORP & Drawdown", tabName = "sorp_x_drawdown"),
            menuItem("SORP + Broken Heart", tabName = "sorp_bh")
        )
    ),
    
    dashboardBody(
        useShinyjs(),
        fluidRow(
            tabItems(
                tabItem(tabName = 'loan_calc', source("source_scripts/loan_calc_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp', source("source_scripts/sorp_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'drawdown', source("source_scripts/drawdown_ui.R", local = TRUE)[1]),
                tabItem(tabName = 'sorp_x_drawdown', source("source_scripts/sorp_x_drawdown_ui.R", local = TRUE)[1]),
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
    source("source_scripts/sorp_x_drawdown_server.R", local = TRUE)[1]
    source("source_scripts/sorp_bh_server.R", local = TRUE)[1]
}

# Run the application 
shinyApp(ui = ui, server = server)