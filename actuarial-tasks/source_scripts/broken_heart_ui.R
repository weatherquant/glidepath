list(
  box(h1("Broken Heart Effect"), width = 12, background = "light-blue"),
  
  box(title = "Parameters", status = "primary", solidHeader = T, width = 6,
      awesomeRadio("bh_gender", "Gender:", choices = list("Male" = 2, "Female" = 1), inline = TRUE),
      numericInputIcon(inputId = "bh_age_current", label = "Current Age:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years")),
      numericInputIcon(inputId = "bh_age_widowed", label = "Age Widowed:", value = 66, min = 66, max = getOmega(BrokenHeart_LifeTable()), icon = list(NULL, "Years"))
      ),
  
  box(title = "Life Expectancy", status = "primary", solidHeader = T, height = "269px",
      h4("Widowed:"),
      h3(textOutput("bh_text_life_ex_widowed")),
      hr(),
      h4("Not Widowed:"),
      h3(textOutput("bh_text_life_ex_not_widowed")),
  ),
  
  div(id = "bh_infobox_female", infoBox(
      title = "Difference in Life Expectancy", h3(textOutput('bh_text_life_ex_diff_female')), width = 12, icon = icon("heart-broken"),
      color = 'fuchsia', fill = F)),
  
  div(id = "bh_infobox_male", infoBox(
      title = "Difference in Life Expectancy", h3(textOutput('bh_text_life_ex_diff_male')), width = 12, icon = icon("heart-broken"),
      color = 'light-blue', fill = F)),
  
  tabBox(type = 'tabs', width = 12,
              tabPanel("Widowed vs Not-Widowed Death Probabilites", plotlyOutput("bh_qx_change_plot")),
              tabPanel("Percentage Increase in Death Probabilites for Widowed Life", plotlyOutput("bh_life_ex_change_plot"))
              )
  )