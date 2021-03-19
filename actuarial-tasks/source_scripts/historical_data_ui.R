list(
 
  box(width = 12, background = "light-blue", h1("Drawdown Simulation Using Historical Data")),
      # column(8,
      #        h1("Drawdown Simulation Using Historical Data")
      #        ),
      #   column(4,
      #         h3(actionLink("code_hist_bt", "View Code", icon = icon("code")))
      #         )
      # ),
 box(width = 12, solidHeader = F, plotlyOutput("hist_drawdown")),
         
                tabBox(type = "tabs", width = 5, height = "505px",
                       
                       tabPanel("Initial Inputs",
                                
                                actionButton(inputId = "hist_resim1", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                                
                                column(width = 8, offset = 2,
                                       br(),
                                       airDatepickerInput("date_start", "Starting Date:", "2000-01", min = "1987-02", max = "2021-01", minView = "months", view = "months", dateFormat = "yyyy-mm-dd")
                                ),
                                column(width = 8, offset = 2,
                                       airDatepickerInput("date_end", "End Date:", "2015-01", min = "1987-02", max = "2021-01", minView = "months", view = "months", dateFormat = "yyyy-mm-dd")
                                ),
                                
                                column(width = 8, offset = 2,
                                       numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro"))
                                ),
                                column(width = 8, offset = 2,
                                       numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 45000, min = 0, icon = icon("euro"))
                                )
 
                       ),
                       
                       tabPanel("Portfolio Weights",
                                
                                actionButton(inputId = "hist_resim2", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                                
                                fluidRow(
                                  br(),br(),
                                  br(),
                                  column(6,  
                                         sliderInput("stock", "Stocks:", min = 0, max = 100, value = 50),
                                         
                                         sliderInput("bond", "Bonds:", min = 0, max = 100, value = 30),
                                  ),
                                  
                                  column(6,
                                         sliderInput("gold", "Gold:", min = 0, max = 100, value = 5),
                                         
                                         sliderInput("realt", "Real Estate:", min = 0, max = 100, value = 5)
                                         
                                  ),
                                  column(6, offset = 3,
                                         sliderInput("rfr", "Risk Free Rate", min = 0, max = 100, value = 10)
                                  )
                                  
                                )
                       ),
                       
                       
                       tabPanel("Adjustments", 
                                actionButton(inputId = "hist_resim3", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                                fluidRow(
                                  br(),br(),
                                  br(),
                                  column(width = 4, offset = 1,
                                         numericInputIcon("stock_inc", "Change in Stocks:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                  
                                  column(width = 4, offset = 1,
                                         numericInputIcon("bond_inc", "Change in Bonds:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                  
                                  column(width = 4, offset = 1,
                                         numericInputIcon("gold_inc", "Change in Gold:",  min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                  
                                  column(width = 4, offset = 1,
                                         numericInputIcon("realt_inc", "Change in RE:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                  
                                  column(width = 4, offset = 1,
                                         numericInputIcon("rfr_inc", "Change in RFR:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                  
                                  column(width = 4, offset = 1,
                                         numericInputIcon("offset", "Begin Adjustments:", min = 0, value = 0, )),
                                  
                                  column(width = 4, offset = 1,
                                         numericInputIcon("wts_timelimit", "Number of Periods for Which Portfolio Changes Will Take Effect:", min = 0, #max = nrow(df.returns)-1,
                                                          value = 0, icon = list(NULL, "Months"))),
                                  
                                  column(width = 4, offset = 1,
                                         strong("Final Fund Allocation Will Be Reached On:", HTML('<br><br>')),
                                         
                                         textOutput("finalfundtime")) 
                                )
                                
                                
                                
                       )
                ),

         
         box(title = "Final Fund Allocation", status = "primary", width = 7, solidHeader = T, height = "505px",
             fluidRow(
               column(12,
                      plotOutput("final_fund")
               ))
             
         )
  
)
