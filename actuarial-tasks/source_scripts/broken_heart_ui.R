list(
  box(h1("Broken Heart Simulator"), width = 12, background = "light-blue"),
  
  box(title = "Parameters:", status = "primary", solidHeader = T, height = "255px",
      awesomeRadio("bh_gender", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      numericInputIcon(inputId = "bh_age_current", label = "Current Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years")),
      numericInputIcon(inputId = "bh_age_widowed", label = "Widowed Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years"))
      ),
  
  box(title = "Life Expectancy", status = "primary", solidHeader = T, 
      h4("Widowed:"),
      h3(textOutput("bh_text_life_ex_widowed")),
      hr(),
      h4("Not Widowed:"),
      h3(textOutput("bh_text_life_ex_not_widowed")),
  ),

    infoBox(
      title = "Difference in Life Expectancy", h3(textOutput('bh_text_life_ex_diff')), width = 12, icon = icon("heart-broken"),
      color = "light-blue", fill = F
    ),

  
  box(title = "Widowed vs Not-Widowed Death Probabilites", status = "primary", solidHeader = T, width = 12,plotlyOutput("bh_qx_change_plot")),
  box(title = "Short-Term Effect of Widowhood on Mortality", status = "primary", solidHeader = T, width = 12,plotlyOutput("bh_life_ex_change_plot"))
 )


  
  