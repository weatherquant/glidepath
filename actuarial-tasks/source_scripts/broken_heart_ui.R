list(
  box(h1("Broken Heart Simulator"), width = 12, background = "light-blue"),
  
  box(title = "Parameters:", status = "primary", solidHeader = T,
      awesomeRadio("bh_gender", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      numericInputIcon(inputId = "bh_age_current", label = "Current Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years")),
      numericInputIcon(inputId = "bh_age_widowed", label = "Widowed Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years"))
      ),
  
  box(title = "Difference in Life Expectancy", status = 'primary', solidHeader = T, width = 7,
      h3(textOutput('bh_text_life_ex_diff'))
      ),
  
  box(title = "Widowed", status = "primary", solidHeader = T,
      h4("Life Expectancy"),
      h3(textOutput("bh_text_life_ex_widowed")),
      ),
  
  box(title = "Not Widowed", status = "primary", solidHeader = T,
      h4("Life Expectancy"),
      h3(textOutput('bh_text_life_ex_not_widowed')),
      ),
  
  tabsetPanel(type = 'tabs',
              tabPanel("Widowed vs Not-Widowed Death Probabilites", style = "margin-top:1em", box(title = "Comparison of Widowed vs Non-Widowed Death Probabilities", status = "primary", width = 12, solidHeader = T, plotlyOutput("bh_qx_change_plot"))),
              tabPanel("Short-Term Effect of Widowhood on Mortality", style = "margin-top:1em", box(title = "Short-Term Effect of Widowhood on the Probability of Death", status = "primary", width = 12, solidHeader = T, plotlyOutput("bh_life_ex_change_plot")))
              )
  )