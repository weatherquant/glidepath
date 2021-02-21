list(
  box(h1("Life Expectancy Visualisations"), width = 12, background = "light-blue"),
  # box(title = "Increase in Expected Lifespan per Year Survived", status = "primary", solidHeader = T, width = 12, plotlyOutput("animated_ex")),
  box(title = "Static Graph", status = "primary", solidHeader = T, width = 12, 
      awesomeRadio("gender_static_ex", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      plotOutput("static_ex")),
  
  box(title = "Moveable Graph", status = "primary", solidHeader = T, width = 12,
      awesomeRadio("gender_interactive_ex", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      sliderInput('current_age_ex', label = "Current Age:", min = 0, max = 105, value = 66),
      h5(strong("Expected Age of Death:")),
      textOutput("text_ex"),
      plotOutput("interactive_ex")
      ),

  box(title = "Comparison Graph", status = "primary", solidHeader = T, width = 12,
      awesomeRadio("gender_comparison_ex", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      sliderInput('ages_ex', label = "Ages to Compare:", min = 55, max = getOmega(ILT15_female_reduced), value = c(66, 80), step = 1),
      plotOutput("comparison_ex")
  )
)