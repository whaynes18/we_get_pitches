library(shiny)
library(shinythemes)
library(kknn)


# Define UI for application
ui <- shinyUI(fluidPage(theme = shinytheme("Spacelab"),
                        
                        # Application title
                        titlePanel("We Get Pitches"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                          sidebarPanel(
                            actionButton('fastball', 'fastball'),
                            actionButton('curve', 'curveball'),
                            actionButton('slider', 'slider'),
                            actionButton('change', 'change-up'),
                            sliderInput(inputId = "speed",
                                        "Pitch Speed:",
                                        min = 50,
                                        max = 110,
                                        value = 90),
                            sliderInput(inputId = "break_angle",
                                        "Break Angle:",
                                        min = -50,
                                        max = 50,
                                        value = 0),
                            sliderInput(inputId = "break_length",
                                        "Break Length:",
                                        min = 0,
                                        max = 15,
                                        value = 6),
                            sliderInput(inputId = "spin_rate",
                                        "Spin Rate:",
                                        min = 0,
                                        max = 3000,
                                        value = 1500),
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
                          ),
                          
                          mainPanel(
                            plotOutput("pitch_plot")
                         
                          )
                        )
))

server <- shinyServer(function(input, output) {
  
  #save output objects to output$
  #what you put into output object should be a render function
  #put braces around code ({ }) to pass code as unified block
  #observeEvent(input$click, {code})
  v <- reactiveValues(data = 5)
  
  ##################################################################
  
  speed_new <- reactiveValues(data = 85)
  breakangle_new <- reactiveValues(data = 2)
  breaklength_new <- reactiveValues(data = 6)
  spin_new <- reactiveValues(data = 2000)
  
  
  ##################################################################
  
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
  
  observeEvent(input$fastball, {
    v$data <- 14
  })
  
  observeEvent(input$curve, {
    v$data <- 14
  })
  
  observeEvent(input$slider, {
    v$data <- 14
  })
  
  observeEvent(input$change, {
    v$data <- 14
    
    })
  
  ########################################################  
  
  
  observeEvent(input$speed, {
    speed_new$data <- input$speed
  })
  
  observeEvent(input$break_angle, {
    breakangle_new$data <- input$break_angle
  })
  
  observeEvent(input$break_length, {
    breaklength_new$data <- input$break_length
  })
  
  observeEvent(input$spin_rate, {
    spin_new$data <- input$spin_rate
  })
  
  
  ########################################################  
  #The call for data
  #speed, break_angle, break_length, spin_rate, v$data
  
  ##test.pitch <- reactive({predict(scale.train.object, data.frame("start_speed" = input$speed, "break_angle" = input$break_angle, "break_length" = input$break_length,
    ##                                 "spin_rate" = input$spin_rate))})
  
  test.pitch <- reactive({predict(scale.train.object, data.frame("start_speed" = speed_new$data, "break_angle" = breakangle_new$data, "break_length" = breaklength_new$data,
                                                                "spin_rate" = spin_new$data))})
 
  
  #test.data <- predict.preProcess(train.preProcess.object, test.pitch())
  
  the.big.guy <- function(zone_id) {
    model <- kknn(filter(pitches.model.data, zone == zone_id)$end ~ start_speed + break_angle + break_length + spin_rate, train = filter(pitches.model.data, zone == zone_id)[-c(13,14)], test = test.pitch(), k = 14)
    m2 <- data.frame(model$prob)
    outcomes <- melt(m2)
    ggplot(outcomes, aes(x = variable, y = value, fill = variable)) + geom_bar(stat = "identity", colour = "black") + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
    
  }
  
  
  ########################################################  
  
  output$pitch_plot <- renderPlot({
    the.big.guy(v$data)
  })
  
  #renderPrint consider for printing out the statistics 
  #reactive builds a reactive object that can be used to store a reactive value in
  
})

# Run the application 
shinyApp(ui = ui, server = server)
