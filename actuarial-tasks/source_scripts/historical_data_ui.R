list(
  box(h1("Historical Data"), width = 12, background = "light-blue"),
  
  column(12,
         box(width = 12, solidHeader = T,
             plotOutput("hist_drawdown")
         ),
         
         column(6,
                tabBox(type = "tabs", width = 12,
                       
                       tabPanel("Initial inputs",
                                fluidRow(
                                  column(width = 6,
                                         numericInputIcon(inputId = "start_capital", label = "Starting Capital:", value = 750000, min = 0, icon = icon("euro"))
                                  ),
                                  column(width = 6,
                                         numericInputIcon(inputId = "annual_withdrawals", label = "Total Withdrawals per Annum:", value = 28000, min = 0, icon = icon("euro"))
                                  ),
                                  column(width = 6,
                                         airDatepickerInput("date_start", "Starting Date:", "2013-01", min = "1987-02", max = "2021-01", minView = "months", view = "months", dateFormat = "yyyy-mm-dd")
                                  ),
                                  column(width = 6,
                                         airDatepickerInput("date_end", "End Date:", "2020-01", min = "1987-02", max = "2021-01", minView = "months", view = "months", dateFormat = "yyyy-mm-dd")
                                  )
                                )
                                
                       ),
                       
                       tabPanel("Portfolio Weight",
                                fluidRow(
                                  column(6,
                                         sliderInput("stock", "Stocks:", min = 0, max = 100, value = 20),
                                         
                                         sliderInput("bond", "Bonds:", min = 0, max = 100, value = 20),
                                  ),
                                  
                                  column(6,
                                         sliderInput("gold", "Gold:", min = 0, max = 100, value = 20),
                                         
                                         sliderInput("realt", "Real Estate:", min = 0, max = 100, value = 20),
                                         
                                         sliderInput("rfr", "Risk Free Rate", min = 0, max = 100, value = 20)
                                  )
                                )
                                
                       ),
                       
                       
                       tabPanel("Adjustments", 
                                fluidRow(column(width = 6,
                                                numericInputIcon("stock_inc", "Change in Stocks:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                         
                                         column(width = 6,
                                                numericInputIcon("bond_inc", "Change in Bonds:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                         
                                         column(width = 6,
                                                numericInputIcon("gold_inc", "Change in Gold:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                         
                                         column(width = 6,
                                                numericInputIcon("realt_inc", "Change in Real Estate:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                         
                                         column(width = 6,
                                                numericInputIcon("rfr_inc", "Change in Risk Free Rate:", min = -100, max = 100, value = 0, icon = list(NULL, icon("percent")))),
                                         
                                         column(width = 6,
                                                numericInputIcon("offset", "Begin Adjustments:", min = 0, value = 0, )),
                                         
                                         column(width = 6,
                                                numericInputIcon("wts_timelimit", "Number of Periods for Which Portfolio Changes Will Take Effect:", min = 0, #max = nrow(df.returns)-1,
                                                                 value = 0, icon = list(NULL, "Months"))),
                                         
                                         column(width = 6,
                                                h5("Final Fund Allocation Will Be Reached On:"),
                                                textOutput("finalfundtime")) 
                                         
                                )
                                
                       )
                )
         ),
         
         box(title = "Final Fund Allocation", status = "primary", width = 6, solidHeader = F,
             fluidRow(
               column(12,
                      plotOutput("final_fund")
               ))
             
         )
  )
)