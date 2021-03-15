list(
  box(h1("Life Expectancy Visualisations"), width = 12, background = "light-blue"),
  box(title = "Percentage Increase in Life Expectancy per Year Survived", status = "primary", solidHeader = T, width = 12, plotlyOutput("percent_increase_ex")),
  
  
  box(title = "Life Expectancy", status = "primary", solidHeader = T, width = 12,
      awesomeRadio("gender_interactive_ex", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      sliderInput('current_age_ex', label = "Current Age:", min = 0, max = 105, value = 66),
      plotOutput("interactive_ex")
      ),

  box(title = "Comparing Life Spans", status = "primary", solidHeader = T, width = 12,
      awesomeRadio("gender_comparison_ex", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      sliderInput('ages_ex', label = "Ages to Compare:", min = 55, max = getOmega(ILT15_female_reduced), value = c(66, 80), step = 1),
      plotOutput("comparison_ex")
  )
)