list(
  box(h1("Life Expectancy Visualisations"), width = 12, background = "light-blue"),
  box(title = "Static Graph", status = "primary", solidHeader = T, width = 12),
  box(title = "Moveable Graph", status = "primary", solidHeader = T, width = 12,
      awesomeRadio("gender_ex", "Gender:", choices = list("Female" = 1, "Male" = 2), inline = TRUE),
      sliderInput('current_age_ex', label = "Current Age:", min = 0, max = 105, value = 66),
      h5(strong("Expected Age of Death:")),
      textOutput("text_ex"),
      plotOutput("interactive_ex")
      )
)