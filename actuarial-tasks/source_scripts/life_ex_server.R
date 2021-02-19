list(
  life_ex <- reactive({
  current_age = input$current_age_ex
  if(input$gender_ex == 1){
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
  
  output$interactive_ex <- renderPlot({
    life_ex <- life_ex()
    life_ex_text = format(round(as.numeric(life_ex), 2), nsmall = 2, big.mark = ",", scientific=FALSE)
    df = data.frame(x = c(0, 5, getOmega(ILT15_female_reduced)), y = c(0, 5, 10))
    p = ggplot(data = df, aes(x = x, y = y)) + 
      xlim(-10, getOmega(ILT15_female_reduced) + 10) + ylim(0, 4) + ylab(NULL)
      # + theme(axis.text.y = element_blank(),
      #       axis.ticks.y = element_blank())
    
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
  })
)