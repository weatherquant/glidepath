list(
  life_ex <- reactive({
  current_age = input$current_age_ex
  if(input$gender_interactive_ex == 1){
    life_ex = current_age + exn(ILT15_female_reduced, current_age)
  } else {
    life_ex = current_age + exn(ILT15_male_reduced, current_age)
  }
  return(life_ex)
  }),
  
  output$text_ex <- renderText({
    ex <- life_ex()
    return(c(format(round(as.numeric(ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE), " Years"))
  }),
  
  # output$animated_ex <- renderPlotly({
  #   ages_x <- unlist(life_table_female[,1])
  #   age_death = numeric(length(ages_x))
  #   for(i in 1:length(ages_x)){
  #     age_death[i] = ages_x[i] + exn(ILT15_female_reduced, ages_x[i])
  #   }
  #   increase_ex = numeric(length(ages_x))
  #   for(i in 1:length(ages_x)){
  #     increase_ex[i] = age_death[i+1] - age_death[i]
  #   }
  #   ex_increase.df = data.frame(x = ages_x, y = increase_ex)
  #   p <- ggplot(data = ex_increase.df, aes(x = x, y = y)) + labs(x = "Age (Years)", y = "Increase in Lifespan") + geom_point(aes(group = seq_along(x)), colour = "hotpink4") + transition_reveal(x)
  #   p <- ggplotly(p)
  #   animation_opts(p)
  # }),
  
  # output$static_ex <- renderPlotly({
  #   ages = c(66, 80)
  #   deaths = numeric(length(ages))
  #   if(input$gender_static_ex == 1){table = ILT15_female_reduced} else {table = ILT15_male_reduced}
  #   for(i in 1:length(ages)){
  #       deaths[i] = ages[i] + exn(table, ages[i])
  #   }
  #   df = data.frame(x = c(ages, deaths), y = c(ages, deaths))
  #   colours = rep(c("red", "hotpink4"), 2)
  #   ggplot(data = df, aes(x = x, y = y)) +
  #     xlim(-10, getOmega(ILT15_female_reduced) + 10) + ylim(0, 4) + xlab("Age") + ylab(NULL) +
  #     geom_vline(aes(xintercept = c(ages, deaths), colour = colours)) + 
  #     theme(axis.text.y = element_blank(),
  #         axis.ticks.y = element_blank())
  #   }),
  
  output$static_ex <- renderPlot({
    ages = c(66, 80)
    deaths = numeric(length(ages))
    if(input$gender_static_ex == 1){table = ILT15_female_reduced} else {table = ILT15_male_reduced}
    for(i in 1:length(ages)){
        deaths[i] = ages[i] + exn(table, ages[i])
    }
    # life_ex_text = format(round(as.numeric(life_ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE)
    df = data.frame(x = c(0, 1, 2, 3, 4), y = c(0, 1, 2, 3, 4))
    p = ggplot(data = df, aes(x = x, y = y)) + 
      xlim(60, getOmega(ILT15_female_reduced)) + ylim(0, 4) + xlab("Age") + ylab(NULL) + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position='none')
    
    xrange <- range(df$x)
    yrange <- range(df$y)
    ratioxy <- diff(xrange) / diff(yrange)
    
    mapping <- aes(x=x,
                   y=y,
                   scale=scale,
                   ratioxy=ratioxy,
                   angleofspine = angleofspine,
                   anglerighthumerus = anglerighthumerus,
                   anglelefthumerus = anglelefthumerus,
                   anglerightradius = anglerightradius,
                   angleleftradius = angleleftradius,
                   anglerightleg =  anglerightleg,
                   angleleftleg = angleleftleg,
                   angleofneck = angleofneck,
                   color = color)
    
    dataman <- data.frame(x = c(ages, deaths), y = rep(2.75, 4),
                          scale = c(1, 1),
                          ratioxy = ratioxy,
                          angleofspine = -pi/2,
                          anglerighthumerus = rep(c(3*pi/2  + pi / 12, pi / 12), 2),
                          anglelefthumerus = rep(c(11*pi / 12, 3*pi/2  - pi / 12), 2),
                          anglerightradius = rep(c(3*pi/2  + pi / 12, 5*pi / 12), 2),
                          angleleftradius = rep(c(7*pi / 12, 3*pi/2  - pi / 12), 2),
                          angleleftleg = 3*pi/2  + pi / 12,
                          anglerightleg = 3*pi/2  - pi / 12,
                          angleofneck = -pi/2,
                          color=c("A","B", "A", "B"))
    p + xkcdman(mapping, dataman) + 
      geom_label(x = c(ages - 10, deaths + 10, mean(c(ages[2], deaths[1]))), y = c(rep(3, 4), 3.75), label = c(ages, deaths, paste0("Difference in Expected Lifespan = ", format(round(as.numeric(deaths[2] - deaths[1]), 2), nsmall = 2, big.mark = ",", scientific=FALSE))))
  }),
  
  output$interactive_ex <- renderPlot({
    life_ex <- life_ex()
    life_ex_text = format(round(as.numeric(life_ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE)
    df = data.frame(x = c(0, 5, getOmega(ILT15_female_reduced)), y = c(0, 5, 10))
    p = ggplot(data = df, aes(x = x, y = y)) + 
      xlim(-10, getOmega(ILT15_female_reduced) + 10) + ylim(0, 4) + xlab("Age") + ylab(NULL) + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position='none')
    
    xrange <- range(df$x)
    yrange <- range(df$y)
    ratioxy <- diff(xrange) / diff(yrange)
    
    mapping <- aes(x=x,
                   y=y,
                   scale=scale,
                   ratioxy=ratioxy,
                   angleofspine = angleofspine,
                   anglerighthumerus = anglerighthumerus,
                   anglelefthumerus = anglelefthumerus,
                   anglerightradius = anglerightradius,
                   angleleftradius = angleleftradius,
                   anglerightleg =  anglerightleg,
                   angleleftleg = angleleftleg,
                   angleofneck = angleofneck,
                   color = color)
    
    dataman <- data.frame(x = c(input$current_age_ex, life_ex), y = c(2.75, 2.75),
                           scale = c(1, 1),
                           ratioxy = ratioxy,
                           angleofspine = -pi/2,
                           anglerighthumerus = c(3*pi/2  + pi / 12, pi / 12),
                           anglelefthumerus = c(11*pi / 12, 3*pi/2  - pi / 12),
                           anglerightradius = c(3*pi/2  + pi / 12, 5*pi / 12),
                           angleleftradius = c(7*pi / 12, 3*pi/2  - pi / 12),
                           angleleftleg = 3*pi/2  + pi / 12,
                           anglerightleg = 3*pi/2  - pi / 12,
                           angleofneck = -pi/2,
                           color=c("A","B"))
    p + xkcdman(mapping, dataman) + 
      geom_label(x = c(input$current_age_ex - 10, life_ex + 10, mean(c(input$current_age_ex, life_ex))), y = c(3, 3, 3.75), label = c(input$current_age_ex, life_ex_text, paste0("Life Expectancy = ", format(round(as.numeric(life_ex - input$current_age_ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE))))
  }),
  
  output$comparison_ex <- renderPlot({
    ages = input$ages_ex
    deaths = numeric(length(ages))
    if(input$gender_comparison_ex == 1){table = ILT15_female_reduced} else {table = ILT15_male_reduced}
    for(i in 1:length(ages)){
      deaths[i] = ages[i] + exn(table, ages[i])
    }
    df = data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, 5))
    p = ggplot(data = df, aes(x = x, y = y)) + 
      xlim(50, getOmega(ILT15_female_reduced) + 5) + ylim(0, 4) + xlab("Age") + ylab(NULL) + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position='none')
    
    xrange <- range(df$x)
    yrange <- range(df$y)
    ratioxy <- diff(xrange) / diff(yrange)
    
    mapping <- aes(x=x,
                   y=y,
                   scale=scale,
                   ratioxy=ratioxy,
                   angleofspine = angleofspine,
                   anglerighthumerus = anglerighthumerus,
                   anglelefthumerus = anglelefthumerus,
                   anglerightradius = anglerightradius,
                   angleleftradius = angleleftradius,
                   anglerightleg =  anglerightleg,
                   angleleftleg = angleleftleg,
                   angleofneck = angleofneck,
                   color = color)
    
    dataman <- data.frame(x = c(ages, deaths), y = rep(2.75, 4),
                          scale = c(1, 1),
                          ratioxy = ratioxy,
                          angleofspine = -pi/2,
                          anglerighthumerus = rep(c(3*pi/2  + pi / 12, pi / 12), 2),
                          anglelefthumerus = rep(c(11*pi / 12, 3*pi/2  - pi / 12), 2),
                          anglerightradius = rep(c(3*pi/2  + pi / 12, 5*pi / 12), 2),
                          angleleftradius = rep(c(7*pi / 12, 3*pi/2  - pi / 12), 2),
                          angleleftleg = 3*pi/2  + pi / 12,
                          anglerightleg = 3*pi/2  - pi / 12,
                          angleofneck = -pi/2,
                          color=c("A","B", "A", "B"))
    p + xkcdman(mapping, dataman) + 
      geom_label(x = c(ages, deaths, mean(c(getOmega(ILT15_female_reduced) + 5, 50))), y = c(rep(3.4, 4), 4), label = c(ages, tommy_round(deaths), paste0("Difference in Lifespan = ", tommy_round(deaths[2] - deaths[1]), " Years")), label.padding = unit(0.4, "lines"), size = c(rep(4, 4), 6.5))
  })
)