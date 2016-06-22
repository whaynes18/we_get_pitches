library(shiny)
library(shinythemes)
library(kknn)
# Define UI for application
ui <- shinyUI(fluidPage(theme = shinytheme("spacelab"),
                        
                        # Application title
                        titlePanel("We Get Pitches"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("pitcher_name", "Select Pitcher:", c("Carlos Zambrano", "Chris Sale", "Randy Johnson")),
                            ###############################################################################
                            ## buttons to select default pitch
                            actionButton('fastball', 'fastball'),
                            actionButton('curve', 'curveball'),
                            actionButton('slider', 'slider'),
                            actionButton('change', 'change-up'),
                            
                            ###############################################################################
                            ## Sliders for pitch characteristics
                            sliderInput(inputId = "speed", "Pitch Speed (mph):", min = 50,max = 110, value = 90),
                            ##sliderInput(inputId = "break_angle","Break Angle (in):",min = -50,max = 50,value = 0),
                            sliderInput(inputId = "break_length", "Break Length (in):",min = 0,max = 15,value = 6),
                            sliderInput(inputId = "pfx_z", "Vertical Change (in):",min = -12,max = 15,value = 6),
                            sliderInput(inputId = "spin_rate","Spin Rate (rpm):", min = 0, max = 3000, value = 1500),
                            
                            
                            ###############################################################################
                            ## Buttons for zones
                            fluidRow(column(1, h4("Zone:"))),
                            fluidRow(
                              column(1, offset = 1, actionButton("zone_11", "11")),
                              column(2, offset = 4, actionButton("zone_12", "12"))),
                            fluidRow(
                              column(1, offset = 2),
                              actionButton("zone_1", "1"),
                              actionButton("zone_2", "2"),
                              actionButton("zone_3", "3")),
                            fluidRow(
                              column(1, offset = 2),
                              actionButton("zone_4", "4"),
                              actionButton("zone_5", "5"),
                              actionButton("zone_6", "6")),
                            fluidRow(
                              column(1, offset = 2),
                              actionButton("zone_7", "7"),
                              actionButton("zone_8", "8"),
                              actionButton("zone_9", "9")),
                            fluidRow(
                              column(1, offset = 1, actionButton("zone_13", "13")),
                              column(2, offset = 4, actionButton("zone_14", "14")))
                            ###############################################################################
                          ),
                          
                          mainPanel(
                            plotOutput("pitch_plot"),
                            plotOutput("pitch_plot_2")
                          )
                        )
))
server <- shinyServer(function(input, output, session) {
  
  #save output objects to output$
  #what you put into output object should be a render function
  #put braces around code ({ }) to pass code as unified block
  #observeEvent(input$click, {code})
  
  ##################################################################
  ## Reactive values for zone and pitch characteristics
  v <- reactiveValues(data = 5)
  speed_new <- reactiveValues(data = 90)
  ##breakangle_new <- reactiveValues(data = 0)
  pfx_z_new <- reactiveValues(data = 6)
  breaklength_new <- reactiveValues(data = 6)
  spin_new <- reactiveValues(data = 1800)
  
  ##################################################################
  
  ##################################################################
  ## Toggle zone selection
  observeEvent(input$zone_1, {
    v$data <- 1
  })
  
  observeEvent(input$zone_2, {
    v$data <- 2
  })
  
  observeEvent(input$zone_3, {
    v$data <- 3
  })
  
  observeEvent(input$zone_4, {
    v$data <- 4
  })  
  
  observeEvent(input$zone_5, {
    v$data <- 5
  })
  
  observeEvent(input$zone_6, {
    v$data <- 6
  })  
  
  observeEvent(input$zone_7, {
    v$data <- 7
  })
  
  observeEvent(input$zone_8, {
    v$data <- 8 
  })  
  
  observeEvent(input$zone_9, {
    v$data <- 9
  })
  
  observeEvent(input$zone_11, {
    v$data <- 11
  })  
  
  observeEvent(input$zone_12, {
    v$data <- 12
  })
  
  observeEvent(input$zone_13, {
    v$data <- 13
  })  
  
  observeEvent(input$zone_14, {
    v$data <- 14
  })
  
  ########################################################  
  ## Default pitch values
  
  fastball <- pitches.clean %>% filter(pitch_type == "FF")
  fastballSpeed <- mean(fastball$start_speed)
  fastballBreak <- mean(fastball$break_length)
  fastballPfx <- mean(fastball$pfx_z)
  fastballSpin <- mean(fastball$spin_rate)
  
  curveball <- pitches.clean %>% filter(pitch_type == "CU")
  curveballSpeed <- mean(curveball$start_speed)
  curveballBreak <- mean(curveball$break_length)
  curveballPfx <- mean(curveball$pfx_z)
  curveballSpin <- mean(curveball$spin_rate)
  
  slider <- pitches.clean %>%  filter(pitch_type == "SL")
  sliderSpeed <- mean(slider$start_speed)
  sliderBreak <- mean(slider$break_length)
  sliderPfx <- mean(slider$pfx_z)
  sliderSpin <- mean(slider$spin_rate)
  
  change <- pitches.clean %>%  filter(pitch_type == "CH")
  changeSpeed <- mean(change$start_speed)
  changeBreak <- mean(change$break_length)
  changePfx <- mean(change$pfx_z)
  changeSpin <- mean(change$spin_rate)
  
  observeEvent(input$fastball, {
    ##speed_new$data <- 90
    ##breaklength_new$data <- 4.19
    ##breakangle_new$data <- 0
    ##pfx_z_new$data <- 6
    ##spin_new$data <- 12
    
    updateSliderInput(session, "speed", value = fastballSpeed)
    updateSliderInput(session, "break_length", value = fastballBreak)
    updateSliderInput(session, "pfx_z", value = fastballPfx)
    updateSliderInput(session, "spin_rate", value = fastballSpin)
  })
  
  observeEvent(input$curve, {
    ##speed_new$data <- 12
    ##breaklength_new$data <- 12
    ##breakangle_new$data <- 12
    ##pfx_z_new$data <- 6
    ##spin_new$data <- 12  
    
    updateSliderInput(session, "speed", value = curveballSpeed)
    updateSliderInput(session, "break_length", value = curveballBreak)
    updateSliderInput(session, "pfx_z", value = curveballPfx)
    updateSliderInput(session, "spin_rate", value = curveballSpin)
  })
  
  observeEvent(input$slider, {
    ##speed_new$data <- 12
    ##breaklength_new$data <- 12
    ##breakangle_new$data <- 12
    ##pfx_z_new$data <- 6
    ##spin_new$data <- 12
    
    updateSliderInput(session, "speed", value = sliderSpeed)
    updateSliderInput(session, "break_length", value = sliderBreak)
    updateSliderInput(session, "pfx_z", value = sliderPfx)
    updateSliderInput(session, "spin_rate", value = sliderSpin)
  })
  
  observeEvent(input$change, {
    ##speed_new$data <- 84
    ##breaklength_new$data <- 7.7
    ##pfx_z_new$data <- 4.0
    ##spin_new$data <- 1525 
    
    
    ##updateSliderInput(session, "speed", value = (speed_new$data <- 84))
    ##updateSliderInput(session, "break_length", value = (breaklength_new$data <- 7.7))
    ##updateSliderInput(session, "pfx_z", value = (pfx_z_new$data <- 4.0))
    ##updateSliderInput(session, "spin_rate", value = (spin_new$data <- 1525))
    
    updateSliderInput(session, "speed", value = changeSpeed)
    updateSliderInput(session, "break_length", value = changeBreak)
    updateSliderInput(session, "pfx_z", value = changePfx)
    updateSliderInput(session, "spin_rate", value = changeSpin)
  })
  
  ########################################################  
  ## Change pitch characteristics with sliders
  observeEvent(input$speed, {
    speed_new$data <- input$speed
  })
  
  ##observeEvent(input$break_angle, {
  ##breakangle_new$data <- input$break_angle
  ##})
  
  observeEvent(input$break_length, {
    breaklength_new$data <- input$break_length
  })
  
  observeEvent(input$pfx_z, {
    pfx_z_new$data <- input$pfx_z
  })
  
  observeEvent(input$spin_rate, {
    spin_new$data <- input$spin_rate
  })
  
  
  ########################################################  
  #The call for data
  #speed, break_angle, break_length, spin_rate, v$data
  
<<<<<<< HEAD

=======
>>>>>>> 79b9cf4b222499ba27d3c6adb66e31d7375947ff
  ##test.pitch <- reactive({predict(scale.train.object, data.frame("start_speed" = input$speed, "break_angle" = input$break_angle, "break_length" = input$break_length,
  ##                                 "spin_rate" = input$spin_rate))})
  
  
  ## Update our "sample pitch" based on input
  ## Use predict to scale values
  ##test.pitch <- reactive({predict(scale.train.object, data.frame("start_speed" = speed_new$data, "break_angle" = breakangle_new$data, "break_length" = breaklength_new$data,
  ##                                                            "spin_rate" = spin_new$data))})
  
<<<<<<< HEAD


=======
>>>>>>> 79b9cf4b222499ba27d3c6adb66e31d7375947ff
  test.pitch <- reactive({predict(scale.train.object, data.frame("start_speed" = speed_new$data, "pfx_z" = pfx_z_new$data, "break_length" = breaklength_new$data,
                                                                 "spin_rate" = spin_new$data))})
  
  ## Function to perform kknn and plot
  ## the.big.guy <- function(zone_id) {
  ## model <- kknn(filter(pitches.model.data, zone == zone_id)$end ~ start_speed + pfx_z + break_length + spin_rate, train = filter(pitches.model.data, zone == zone_id)[-c(13,14)], test = test.pitch(), k = 14)
  
  ##m2 <- data.frame(model$prob)
  ##outcomes <- melt(m2)
  ##ggplot(outcomes, aes(x = variable, y = value, fill = variable)) + geom_bar(stat = "identity", colour = "black") + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
  ##}
  
  ########################################################  
  
  
  pitcher.find <- function(pitcher){
    (pitcher)
    pitches.model.data$match <- str_count(pitcher, pitches.model.data$pitcher_name) 
    pitches.model.data %>% filter(match == 1)
  }
  
  the.big.guy.R.pitcher <- function(zone_id, pitcher = "NA") {
    if (pitcher != "NA") {
      relevant.data <- filter(pitcher.find(pitcher), zone == zone_id, stand == "R")
      relevant.data <- relevant.data[,-9]
    } else {
      relevant.data <- filter(pitches.model.data, zone == zone_id, stand == "R")
    }
    model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
    m2 <- data.frame(model$prob)
    outcomes <- melt(m2)
    outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
    ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
  }
  
  the.big.guy.L.pitcher <- function(zone_id, pitcher = "NA") {
    if (pitcher != "NA") {
      relevant.data <- filter(pitcher.find(pitcher), zone == zone_id, stand == "L")
      relevant.data <- relevant.data[,-9]
    } else {
      relevant.data <- filter(pitches.model.data, zone == zone_id, stand == "L")
    }
    model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
    m2 <- data.frame(model$prob)
    outcomes <- melt(m2)
    outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
<<<<<<< HEAD

    ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution - Lefty Hitters")

    ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")

=======
    ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
>>>>>>> 79b9cf4b222499ba27d3c6adb66e31d7375947ff
  }
  
  
  ########################################################  
  
  ## Plot output
  output$pitch_plot <- renderPlot({
    the.big.guy.R.pitcher(v$data)
  })
  
  output$pitch_plot_2 <- renderPlot({
    the.big.guy.L.pitcher(v$data)
  })
  
  
  #renderPrint consider for printing out the statistics 
  #reactive builds a reactive object that can be used to store a reactive value in
  
})
# Run the application 
shinyApp(ui = ui, server = server)