library(shiny)
library(shinythemes)
library(kknn)
library(shinyjs)
library(dplyr)
library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)
library(kknn)
library(reshape2)
load("data/coordinates.RData")
load("data/app3.Rdata")
load("data/pitcher_names.rdata")


# Define UI for application
ui <- shinyUI(fluidPage(theme = shinytheme("spacelab"),
                        
                        
                        # app logo
                        tags$head(tags$link(rel="shortcut icon", href= "http://www.harrycarays.com/content/uploads/2015/07/MLB-ball-back.png")),
                        # Application title
                        titlePanel("We Get Pitches"),
                        useShinyjs(),
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("pitcher_name", "Select Pitcher:", c("All", relevant.names)),
                            selectInput("other_pitcher", "Select Pitcher for Comparison:", c("None", "All", relevant.names)),
                            ###############################################################################
                            ## buttons to select default pitch
                            actionButton('fastball', 'four-seam fastball'),
                            actionButton('slider', 'slider'),
                            actionButton('twoSeam', 'two-seam fastball'),
                            actionButton('change', 'change-up'),
                            actionButton('curve', 'curveball'),
                            actionButton('sinker','sinker'),
                            actionButton('cutter','cutter'),
                            actionButton('knuckleCurve','knuckle-curve'),
                            actionButton('knuckleball', 'knuckleball'),
                            actionButton('eephus','eephus'),
                            
                            ###############################################################################
                            ## Sliders for pitch characteristics
                            uiOutput("speed_slider"),
                            uiOutput("break_slider"),
                            uiOutput("pfx_z_slider"),
                            uiOutput("spin_rate_slider"),
                            
                            # create sliders in order to track the stats of the comparison pitcher. We do not 
                            # actually want to see these sliders so we will hide them away.
                            hidden(sliderInput(inputId = "otherSpeed", "Other Pitch Speed (mph):", min = 50, max = 100, value = 90)),
                            hidden(sliderInput(inputId = "otherBreak_length", "Break Length (in):",min = 0,max = 15,value = 6)),
                            hidden(sliderInput(inputId = "otherPfx_z", "Vertical Change (in):",min = -12,max = 15,value = 6)),
                            hidden(sliderInput(inputId = "otherSpin_rate","Spin Rate (rpm):", min = 0, max = 3000, value = 1500)),
                            fluidRow(column(1, offset = 4, actionButton("all_zones", "All Zones"))),
                            
                            plotOutput("pitch_plot_3", click = "plot_click")
                          ),  
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Pitch Outcome Distributions", plotOutput("pitch_plot"), plotOutput("pitch_plot_2")),
                              tabPanel("Summary Statistics", verbatimTextOutput("accuracy"))
                            )
                            
                          )
                        )
                        
))
server <- shinyServer(function(input, output, session) {
  
  #save output objects to output$
  #what you put into output object should be a render function
  #put braces around code ({ }) to pass code as unified block
  #observeEvent(input$click, {code})

  
  ##################################################################
 
  
  
  output$speed_slider <- renderUI({
    if (input$pitcher_name == "All") {
      max.speed = 110
      min.speed = 60
    } else {
      max.speed <- max(filter(pitches.clean, pitcher_name == input$pitcher_name)$start_speed)
      min.speed <- min(filter(pitches.clean, pitcher_name == input$pitcher_name)$start_speed)
    }
    sliderInput(inputId = "speed", "Pitch Speed (mph):", min = round(min.speed,1), max = round(max.speed,1), step = round((max.speed - min.speed)/30, 1), value = 90, round = TRUE)
  })
  
  output$break_slider <- renderUI({
    if (input$pitcher_name == "All") {
      max.break = 15
      min.break = 0
    } else {
      max.break <- max(filter(pitches.clean, pitcher_name == input$pitcher_name)$break_length)
      min.break <- min(filter(pitches.clean, pitcher_name == input$pitcher_name)$break_length)
    }
    sliderInput(inputId = "break_length", "Break Length (in)", min = round(min.break,1), max = round(max.break,1), step = round((max.break - min.break)/30,1), value = 6, round = TRUE)
  })
  
  output$pfx_z_slider <- renderUI({
    if (input$pitcher_name == "All") {
      max.break.height = 15
      min.break.height = -12
    } else {
      max.break.height <- max(filter(pitches.clean, pitcher_name == input$pitcher_name)$pfx_z)
      min.break.height <- min(filter(pitches.clean, pitcher_name == input$pitcher_name)$pfx_z)
    }
    sliderInput(inputId = "pfx_z", "Vertical Change (in)", min = round(min.break.height, 1),max = round(max.break.height,1), step = round((max.break.height - min.break.height)/30, 1), value = 6, round = TRUE)
  })
  
  output$spin_rate_slider <- renderUI({
    if (input$pitcher_name == "All") {
      max.spin.rate = 3000
      min.spin.rate = 0
    } else {
      max.spin.rate <- max(filter(pitches.clean, pitcher_name == input$pitcher_name)$spin_rate)
      min.spin.rate <- min(filter(pitches.clean, pitcher_name == input$pitcher_name)$spin_rate)
    }
    sliderInput(inputId = "spin_rate", "Spin Rate (rpm)", min = round(min.spin.rate,1), max = round(max.spin.rate,1), step = round((max.spin.rate - min.spin.rate)/30, 1), value = 1500, round = TRUE)
  })
  
  zones <- reactiveValues(data = 0)
  speed_new <- reactiveValues(data = 90)
  pfx_z_new <- reactiveValues(data = 6)
  breaklength_new <- reactiveValues(data = 6)
  spin_new <- reactiveValues(data = 1800)
  speed_new2 <- reactiveValues(data = 90)
  pfx_z_new2 <- reactiveValues(data = 6)
  breaklength_new2 <- reactiveValues(data = 6)
  spin_new2 <- reactiveValues(data = 1800)
  click_point_x <- reactiveValues(data = NULL)
  click_point_y <- reactiveValues(data = NULL)
  
  
  
  ##################################################################

  ########################################################  
  ## Default pitch values
  
  observeEvent(input$plot_click, {
    zones$data <- 0
    click_point_x$data <- input$plot_click$x
    click_point_y$data <- input$plot_click$y
  })
  
  observeEvent(input$all_zones, {
    zones$data <- 1
  })
  
  observeEvent(input$pitcher_name, {
    pitcher.df <- pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name)
    levels(pitcher.df$pitch_type) <- c("change", "curve", "eephus", "cutter", "fastball", "sinker", "twoSeam", "knuckleCurve", "knuckleball", "slider")
    pitches.all <- levels(pitcher.df$pitch_type)
    # These next two lines get it so that the levels of the pitch type have only the 
    # specified pitchers pitch type
    pitcher.df$pitch_type <- as.character(pitcher.df$pitch_type)
    pitcher.df$pitch_type <- as.factor(pitcher.df$pitch_type)
    pitches.specified <- levels(pitcher.df$pitch_type)
    
    
    if (input$pitcher_name != "All"){
      # we first want to disable all pitches, in case they were enabled from a previous pitcher
      for (pitch in pitches.all){
        disable(pitch)
      }
      # then we want to enable the specific pitchers to a pitcher
      for (pitch in pitches.specified){
        enable(pitch)
      }
    }
    else{
      # if pitcher does = all, enable all pitches
      for (pitch in pitches.all){
        enable(pitch)
      }
    }
  })
  
  observeEvent(input$fastball, {
    
    if (input$pitcher_name == "All"){
      fastball <- pitches.clean %>% dplyr::filter(pitch_type == "FF")
    }
    else{
      fastball <- pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "FF")
      
    }
    fastballSpeed <- mean(fastball$start_speed)
    fastballBreak <- mean(fastball$break_length)
    fastballPfx <- mean(fastball$pfx_z)
    fastballSpin <- mean(fastball$spin_rate)
    
    updateSliderInput(session, "speed", value = fastballSpeed)
    updateSliderInput(session, "break_length", value = fastballBreak)
    updateSliderInput(session, "pfx_z", value = fastballPfx)
    updateSliderInput(session, "spin_rate", value = fastballSpin)
    
    if (input$other_pitcher == "All"){
      fastballOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "FF")
    }
    else{
      fastballOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "FF")
    }
    
    fastballOtherSpeed <- mean(fastballOther$start_speed)
    fastballOtherBreak <- mean(fastballOther$break_length)
    fastballOtherPfx <- mean(fastballOther$pfx_z)
    fastballOtherSpin <- mean(fastballOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = fastballOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = fastballOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = fastballOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = fastballOtherSpin)
    
  })
  
  observeEvent(input$slider, {
    
    if (input$pitcher_name == "All"){
      sliders = pitches.clean %>% dplyr::filter(pitch_type == "SL")
    }
    else{
      sliders = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "SL")
    }
    sliderSpeed = mean(sliders$start_speed)
    sliderBreak = mean(sliders$break_length)
    sliderPfx = mean(sliders$pfx_z)
    sliderSpin = mean(sliders$spin_rate)
    
    updateSliderInput(session, "speed", value = sliderSpeed)
    updateSliderInput(session, "break_length", value = sliderBreak)
    updateSliderInput(session, "pfx_z", value = sliderPfx)
    updateSliderInput(session, "spin_rate", value = sliderSpin)
    
    if (input$other_pitcher == "All"){
      sliderOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "SL")
    }
    else{
      sliderOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "SL")
    }
    
    sliderOtherSpeed <- mean(sliderOther$start_speed)
    sliderOtherBreak <- mean(sliderOther$break_length)
    sliderOtherPfx <- mean(sliderOther$pfx_z)
    sliderOtherSpin <- mean(sliderOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = sliderOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = sliderOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = sliderOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = sliderOtherSpin)
  })
  
  observeEvent(input$twoSeam, {
    
    if (input$pitcher_name == "All"){
      twoSeams = pitches.clean %>% dplyr::filter(pitch_type == "FT")
    }
    else{
      twoSeams = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "FT")
    }
    twoSeamSpeed = mean(twoSeams$start_speed)
    twoSeamBreak = mean(twoSeams$break_length)
    twoSeamPfx = mean(twoSeams$pfx_z)
    twoSeamSpin = mean(twoSeams$spin_rate)
    
    updateSliderInput(session, "speed", value = twoSeamSpeed)
    updateSliderInput(session, "break_length", value = twoSeamBreak)
    updateSliderInput(session, "pfx_z", value = twoSeamPfx)
    updateSliderInput(session, "spin_rate", value = twoSeamSpin)
    
    if (input$other_pitcher == "All"){
      twoSeamOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "FT")
    }
    else{
      twoSeamOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "FT")
    }
    
    twoSeamOtherSpeed <- mean(twoSeamOther$start_speed)
    twoSeamOtherBreak <- mean(twoSeamOther$break_length)
    twoSeamOtherPfx <- mean(twoSeamOther$pfx_z)
    twoSeamOtherSpin <- mean(twoSeamOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = twoSeamOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = twoSeamOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = twoSeamOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = twoSeamOtherSpin)
  })
  
  observeEvent(input$change, {
    
    if(input$pitcher_name == "All"){
      changes = pitches.clean %>% dplyr::filter(pitch_type == "CH")
    }
    else{
      changes = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "CH")
    }
    changeSpeed <- mean(changes$start_speed)
    changeBreak <- mean(changes$break_length)
    changePfx <- mean(changes$pfx_z)
    changeSpin <- mean(changes$spin_rate)
    
    updateSliderInput(session, "speed", value = changeSpeed)
    updateSliderInput(session, "break_length", value = changeBreak)
    updateSliderInput(session, "pfx_z", value = changePfx)
    updateSliderInput(session, "spin_rate", value = changeSpin)
    
    if (input$other_pitcher == "All"){
      changeOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "CH")
    }
    else{
      changeOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "CH")
    }
    
    changeOtherSpeed <- mean(changeOther$start_speed)
    changeOtherBreak <- mean(changeOther$break_length)
    changeOtherPfx <- mean(changeOther$pfx_z)
    changeOtherSpin <- mean(changeOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = changeOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = changeOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = changeOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = changeOtherSpin)
  })
  
  observeEvent(input$curve, {
    
    if (input$pitcher_name == "All"){
      curves = pitches.clean %>% dplyr::filter(pitch_type == "CU")
    }
    else{
      curves = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "CU")
    }
    curveballSpeed <- mean(curves$start_speed)
    curveballBreak <- mean(curves$break_length)
    curveballPfx <- mean(curves$pfx_z)
    curveballSpin <- mean(curves$spin_rate)
    
    updateSliderInput(session, "speed", value = curveballSpeed)
    updateSliderInput(session, "break_length", value = curveballBreak)
    updateSliderInput(session, "pfx_z", value = curveballPfx)
    updateSliderInput(session, "spin_rate", value = curveballSpin)
    
    if (input$other_pitcher == "All"){
      curveOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "CU")
    }
    else{
      curveOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "CU")
    }
    
    curveOtherSpeed <- mean(curveOther$start_speed)
    curveOtherBreak <- mean(curveOther$break_length)
    curveOtherPfx <- mean(curveOther$pfx_z)
    curveOtherSpin <- mean(curveOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = curveOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = curveOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = curveOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = curveOtherSpin)
  })
  
  observeEvent(input$sinker, {
    
    if(input$pitcher_name == "All"){
      sinkers = pitches.clean %>% dplyr::filter(pitch_type == "SI")
    }
    else{
      sinkers = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "SI")
    }
    sinkerSpeed <- mean(sinkers$start_speed)
    sinkerBreak <- mean(sinkers$break_length)
    sinkerPfx <- mean(sinkers$pfx_z)
    sinkerSpin <- mean(sinkers$spin_rate)
    
    updateSliderInput(session, "speed", value = sinkerSpeed)
    updateSliderInput(session, "break_length", value = sinkerBreak)
    updateSliderInput(session, "pfx_z", value = sinkerPfx)
    updateSliderInput(session, "spin_rate", value = sinkerSpin)
    
    if (input$other_pitcher == "All"){
      sinkerOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "SI")
    }
    else{
      sinkerOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "SI")
    }
    
    sinkerOtherSpeed <- mean(sinkerOther$start_speed)
    sinkerOtherBreak <- mean(sinkerOther$break_length)
    sinkerOtherPfx <- mean(sinkerOther$pfx_z)
    sinkerOtherSpin <- mean(sinkerOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = sinkerOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = sinkerOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = sinkerOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = sinkerOtherSpin)
  })
  
  observeEvent(input$cutter, {
    
    if(input$pitcher_name == "All"){
      cutters = pitches.clean %>% dplyr::filter(pitch_type == "FC")
    }
    else{
      cutters = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "FC")
    }
    cutterSpeed <- mean(cutters$start_speed)
    cutterBreak <- mean(cutters$break_length)
    cutterPfx <- mean(cutters$pfx_z)
    cutterSpin <- mean(cutters$spin_rate)
    
    updateSliderInput(session, "speed", value = cutterSpeed)
    updateSliderInput(session, "break_length", value = cutterBreak)
    updateSliderInput(session, "pfx_z", value = cutterPfx)
    updateSliderInput(session, "spin_rate", value = cutterSpin)
    
    if (input$other_pitcher == "All"){
      cutterOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "FC")
    }
    else{
      cutterOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "FC")
    }
    
    cutterOtherSpeed <- mean(cutterOther$start_speed)
    cutterOtherBreak <- mean(cutterOther$break_length)
    cutterOtherPfx <- mean(cutterOther$pfx_z)
    cutterOtherSpin <- mean(cutterOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = cutterOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = cutterOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = cutterOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = cutterOtherSpin)
  })
  
  observeEvent(input$knuckleCurve, {
    
    if(input$pitcher_name == "All"){
      knuckleCurves = pitches.clean %>% dplyr::filter(pitch_type == "KC")
    }
    else{
      knuckleCurves = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "KC")
    }
    knuckleCurveSpeed <- mean(knuckleCurves$start_speed)
    knuckleCurveBreak <- mean(knuckleCurves$break_length)
    knuckleCurvePfx <- mean(knuckleCurves$pfx_z)
    knuckleCurveSpin <- mean(knuckleCurves$spin_rate)
    
    updateSliderInput(session, "speed", value = knuckleCurveSpeed)
    updateSliderInput(session, "break_length", value = knuckleCurveBreak)
    updateSliderInput(session, "pfx_z", value = knuckleCurvePfx)
    updateSliderInput(session, "spin_rate", value = knuckleCurveSpin)
    
    if (input$other_pitcher == "All"){
      knuckleCurveOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "KC")
    }
    else{
      knuckleCurveOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "KC")
    }
    
    knuckleCurveOtherSpeed <- mean(knuckleCurveOther$start_speed)
    knuckleCurveOtherBreak <- mean(knuckleCurveOther$break_length)
    knuckleCurveOtherPfx <- mean(knuckleCurveOther$pfx_z)
    knuckleCurveOtherSpin <- mean(knuckleCurveOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = knuckleCurveOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = knuckleCurveOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = knuckleCurveOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = knuckleCurveOtherSpin)
  })
  
  observeEvent(input$knuckleball, {
    
    if(input$pitcher_name == "All"){
      knuckleballs = pitches.clean %>% dplyr::filter(pitch_type == "KN")
    }
    else{
      knuckleballs = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "KN")
    }
    knuckleballSpeed <- mean(knuckleballs$start_speed)
    knuckleballBreak <- mean(knuckleballs$break_length)
    knuckleballPfx <- mean(knuckleballs$pfx_z)
    knuckleballSpin <- mean(knuckleballs$spin_rate)
    
    updateSliderInput(session, "speed", value = knuckleballSpeed)
    updateSliderInput(session, "break_length", value = knuckleballBreak)
    updateSliderInput(session, "pfx_z", value = knuckleballPfx)
    updateSliderInput(session, "spin_rate", value = knuckleballSpin)
    
    if (input$other_pitcher == "All"){
      knuckleballOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "KN")
    }
    else{
      knuckleballOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "KN")
    }
    
    knuckleballOtherSpeed <- mean(knuckleballOther$start_speed)
    knuckleballOtherBreak <- mean(knuckleballOther$break_length)
    knuckleballOtherPfx <- mean(knuckleballOther$pfx_z)
    knuckleballOtherSpin <- mean(knuckleballOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = knuckleballOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = knuckleballOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = knuckleballOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = knuckleballOtherSpin)
  })
  
  observeEvent(input$eephus, {
    
    if(input$pitcher_name == "All"){
      eephuss = pitches.clean %>% dplyr::filter(pitch_type == "EP")
    }
    else{
      eephuss = pitches.clean %>% dplyr::filter(pitcher_name == input$pitcher_name) %>% dplyr::filter(pitch_type == "EP")
    }
    eephusSpeed <- mean(eephuss$start_speed)
    eephusBreak <- mean(eephuss$break_length)
    eephusPfx <- mean(eephuss$pfx_z)
    eephusSpin <- mean(eephuss$spin_rate)
    
    updateSliderInput(session, "speed", value = eephusSpeed)
    updateSliderInput(session, "break_length", value = eephusBreak)
    updateSliderInput(session, "pfx_z", value = eephusPfx)
    updateSliderInput(session, "spin_rate", value = eephusSpin)
    
    if (input$other_pitcher == "All"){
      eephusOther <- pitches.clean %>% dplyr::filter(pitcher_name != input$pitcher_name) %>% dplyr::filter(pitch_type == "EP")
    }
    else{
      eephusOther <- pitches.clean %>% dplyr::filter(pitcher_name == input$other_pitcher) %>% dplyr::filter(pitch_type == "EP")
    }
    
    eephusOtherSpeed <- mean(eephusOther$start_speed)
    eephusOtherBreak <- mean(eephusOther$break_length)
    eephusOtherPfx <- mean(eephusOther$pfx_z)
    eephusOtherSpin <- mean(eephusOther$spin_rate)
    
    updateSliderInput(session, "otherSpeed", value = eephusOtherSpeed)
    updateSliderInput(session, "otherBreak_length", value = eephusOtherBreak)
    updateSliderInput(session, "otherPfx_z", value = eephusOtherPfx)
    updateSliderInput(session, "otherSpin_rate", value = eephusOtherSpin)
  })
  
  
  
  ########################################################  
  ## Change pitch characteristics with sliders
  observeEvent(input$speed, {
    speed_new$data <- input$speed
  })
  
  observeEvent(input$otherSpeed, {
    speed_new2$data <- input$otherSpeed
  })
  
  observeEvent(input$break_length, {
    breaklength_new$data <- input$break_length
  })
  
  observeEvent(input$otherBreak_length, {
    breaklength_new2$data <- input$otherBreak_length
  })
  
  observeEvent(input$pfx_z, {
    pfx_z_new$data <- input$pfx_z
  })
  
  observeEvent(input$otherPfx_z, {
    pfx_z_new2$data <- input$otherPfx_z
  })
  
  observeEvent(input$spin_rate, {
    spin_new$data <- input$spin_rate
  })
  
  observeEvent(input$otherSpin_rate, {
    spin_new2$data <- input$otherSpin_rate
  })
  
  ########################################################  
  
  coordinate <- reactive({
    data.frame("x_val" = click_point_x$data, "y_val" = click_point_y$data)
  })
  
  test.pitch <- reactive({predict(scale.train.object, data.frame("start_speed" = speed_new$data, "pfx_z" = pfx_z_new$data, "break_length" = breaklength_new$data,
                                                                 "spin_rate" = spin_new$data))})
  
  test.pitch2 <- reactive({predict(scale.train.object, data.frame("start_speed" = speed_new2$data, "pfx_z" = pfx_z_new2$data, "break_length" = breaklength_new2$data,
                                                                  "spin_rate" = spin_new2$data))})
  

  ########################################################  
  
  create_radii <- function(x, y, x_mid, y_mid){
    value <- sqrt((x - x_mid)^2 + (y - y_mid)^2)
    value
  }
  
  calculate_distances <- function(start_speed, pfx_z, break_length, spin_rate){
    dist.df <- data.frame(start_speed, break_length, spin_rate, pfx_z)
    cmon <- test.pitch()
    pop <- rbind(dist.df, cmon)
    value <- dist(pop)
    value[1]
  }

  the.big.guy.pitcher.radii <- function(pitcher = "All", other_pitcher = "None", stance) {

    # set up the coordinate system based on the reactive function
    coords <- coordinate()
    x_val <- coords$x_val
    y_val <- coords$y_val

    
    # set up the title for the graph
    if (stance == "R"){
      title = "Comparison Pitch Outcome Distribution - Righty Hitters"
    }
    else{
      title = "Comparison Pitch Outcome Distribution - Lefty Hitters"
    }
    
    if (pitcher != "All") {
      if (other_pitcher != "None") {
        if (other_pitcher == "All"){
          relevant.data <- filter(pitches.model.data.2, pitcher_name == pitcher, stand == stance)
          relevant.data.2 <- filter(pitches.model.data.2, pitcher_name != pitcher, stand == stance)
        }
        else{
          relevant.data <- filter(pitches.model.data.2, pitcher_name == pitcher, stand == stance)
          relevant.data.2 <- filter(pitches.model.data.2, pitcher_name == other_pitcher, stand == stance)
        }
        
        # caculate radius only if the "All Zones" button is not selected
        if (zones$data == 0 & !(is.null(click_point_x$data))){
          relevant.data$radius <- create_radii(relevant.data$x, relevant.data$y, x_val, y_val)
          relevant.data <- relevant.data %>% dplyr::filter(radius < 8)
          
          relevant.data.2$radius <- create_radii(relevant.data.2$x, relevant.data.2$y, x_val, y_val)
          relevant.data.2 <- relevant.data.2 %>% dplyr::filter(radius < 8)
          
        }

        model.pitcher <- kknn(relevant.data$end ~ ., train = relevant.data[c(1:4, 9)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
        model.all <- kknn(relevant.data.2$end ~ ., train = relevant.data.2[c(1:4, 9)], test = test.pitch2(), k = sqrt(nrow(relevant.data.2)))
        
        # Get the points that are used in the model, then calculate the average euclidean distance
        # between these points and the actual pitch.
        used.points <- model.pitcher$C
        used <- relevant.data[used.points,]
        used$euc_distances <- calculate_distances(used$start_speed, used$pfx_z, used$break_length, used$spin_rate)
        average_euc <- sum(used$euc_distances) / nrow(used)
        
        used.points.2 <- model.all$C
        used.2 <- relevant.data[used.points.2,]
        used.2$euc_distances <- calculate_distances(used.2$start_speed, used.2$pfx_z, used.2$break_length, used.2$spin_rate)
        average_euc.2 <- sum(used.2$euc_distances) / nrow(used.2)
        
        pitcher.probs <- as.numeric(model.pitcher$prob)
        all.probs <- as.numeric(model.all$prob)
        outcomeLevels <- levels(pitches.model.data.2$end)
        both.probs <- data.frame(outcomeLevels, pitcher.probs, all.probs)
        
        outcomes <- melt(both.probs)
        all <- outcomes %>% filter(variable == "all.probs")
        selected.pitcher <- outcomes %>% filter(variable == "pitcher.probs")
        
        selected.pitcher$other <- selected.pitcher$outcomeLevels
        all$other <- "League Average"
        outcomes <- rbind(selected.pitcher, all)
        
        outcomes$outcomeLevels <- factor(all$outcomeLevels,levels(all$outcomeLevels)[c(10, 2, 4, 1, 5, 8, 3, 7, 9, 11, 6)])
        selected.pitcher$outcomeLevels <- factor(selected.pitcher$outcomeLevels,levels(selected.pitcher$outcomeLevels)[c(10, 2, 4, 1, 5, 8, 3, 7, 9, 11, 6)])
        
        
        
        plot <- ggplot(outcomes, aes(x = outcomeLevels, y = value, fill = other)) + theme_bw() + 
          theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) + 
          geom_bar(colour = "black", stat = "identity", position = "dodge") + scale_fill_manual(values = c("yellow1", "green3", "red3", "green3", "blue3", "red3", "blue3", "blue3", "red3", "green3", "red3", "grey75")) + 
          ylab("Probability") + xlab("") + ggtitle(title) + guides(fill = FALSE)
        
        k_value <- sqrt(nrow(relevant.data))
        k_value_2 <- sqrt(nrow(relevant.data.2))
        
        return_vals <- list(plot, k_value, k_value_2, average_euc, average_euc.2)
        return_vals
      }
      else{
        
        relevant.data <- filter(pitches.model.data.2, pitcher_name == pitcher, stand == stance)
        
        # this loop will calculate radii for every point in the data set
        if (zones$data == 0 & !(is.null(click_point_x$data))){
          relevant.data$radius <- create_radii(relevant.data$x, relevant.data$y, x_val, y_val)
          relevant.data <- relevant.data %>% dplyr::filter(radius < 8)
        }
        
        # k can just equal the number of points in relevant.data now because we already filtered it down
        model <- kknn(relevant.data$end ~ ., train = relevant.data[c(1:4, 9)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
        
        used.points <- model$C
        used <- relevant.data[used.points,]
        used$euc_distances <- calculate_distances(used$start_speed, used$pfx_z, used$break_length, used$spin_rate)
        average_euc <- sum(used$euc_distances) / nrow(used)
        
        m2 <- data.frame(model$prob)
        outcomes <- melt(m2)
        outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
        
        
        plot <- ggplot(outcomes, aes(x = variable, y = value)) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) +
          theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) +
          guides(fill = FALSE) + scale_fill_manual(values = c("green3", "green3", "green3","yellow1", "blue3", "blue3", "blue3", "blue3","red3", "red3", "red3")) + 
          ylab("Probability") + xlab("") + ggtitle(title)
        k_value <- sqrt(nrow(relevant.data))
        
        return_vals <- list(plot, k_value, average_euc)
        return_vals
      }
    } 
    else {
      
      relevant.data <- filter(pitches.model.data.2, stand == stance)
      if (zones$data == 0 & !(is.null(click_point_x$data))){
        relevant.data$radius <- create_radii(relevant.data$x, relevant.data$y, x_val, y_val)
        relevant.data <- relevant.data %>% dplyr::filter(radius < 8)
      }
      

      model <- kknn(relevant.data$end ~ ., train = relevant.data[c(1:4, 9)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
      
      used.points <- model$C
      used <- relevant.data[used.points,]
      used$euc_distances <- calculate_distances(used$start_speed, used$pfx_z, used$break_length, used$spin_rate)
      average_euc <- sum(used$euc_distances) / nrow(used)
      
      m2 <- data.frame(model$prob)
      outcomes <- melt(m2)
      outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
      
      
      plot <- ggplot(outcomes, aes(x = variable, y = value)) + geom_bar(stat = "identity", colour = "black", aes(fill = variable))+
        theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) +
        guides(fill = FALSE) + scale_fill_manual(values = c("green3", "green3", "green3","yellow1", "blue3", "blue3", "blue3", "blue3","red3", "red3", "red3")) + 
        ylab("Probability") + xlab("") + ggtitle(title)
      k_value <- sqrt(nrow(relevant.data))

      return_vals <- list(plot, k_value, average_euc)
      return_vals
    }
  }
  
  strike.zone.plot <- function(){
    
    # set up the coordinate system based on the reactive function
    coords <- coordinate()
    x_val <- coords$x_val
    y_val <- coords$y_val
    
    angle <- seq(-pi, pi, length = 50)
    df <- data.frame(x = x_val + 8*sin(angle), y = y_val + 8*cos(angle))
    
    if (is.null(click_point_x$data) | (zones$data == 1)){
      ggplot(NULL, aes(x = x, y = y))  + geom_point(data = coordinates, aes(color = strikeZone)) + scale_color_manual(values = c("gray77", "royalblue2")) +
        theme(axis.line=element_blank(), plot.margin = unit( c(0,0,0,0) , "in" ), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", panel.background=element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_rect(fill = "grey96", color = "grey96"), title = element_text(size = 28)) +
        geom_segment(aes(x = 90, y = 147, xend = 90, yend = 194)) + geom_segment(aes(x = 108, y = 147, xend = 108, yend = 194)) +
        geom_segment(aes(x = 126, y = 147, xend = 126, yend = 194)) + geom_segment(aes(x = 143, y = 147, xend = 143, yend = 194)) + 
        geom_segment(aes(x = 90, y = 147, xend = 143, yend = 147)) + geom_segment(aes(x = 90, y = 163, xend = 143, yend = 163)) + 
        geom_segment(aes(x = 90, y = 179, xend = 143, yend = 179)) + geom_segment(aes(x = 90, y = 194, xend = 143, yend = 194)) +
        geom_segment(aes(x = max(coordinates$x), y = min(coordinates$y), xend = max(coordinates$x), yend = max(coordinates$y))) + geom_segment(aes(x = min(coordinates$x), y = min(coordinates$y), xend = min(coordinates$x), yend = max(coordinates$y))) + 
        geom_segment(aes(x = min(coordinates$x), y = max(coordinates$y), xend = max(coordinates$x), yend = max(coordinates$y))) + geom_segment(aes(x = min(coordinates$x), y = min(coordinates$y), xend = max(coordinates$x), yend = min(coordinates$y)))
    }
    else{
      ggplot(NULL, aes(x = x, y = y))  + geom_point(data = coordinates, aes(color = strikeZone)) + scale_color_manual(values = c("gray77", "royalblue2")) +
        theme(axis.line=element_blank(), plot.margin = unit( c(0,0,0,0) , "in" ), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", panel.background=element_blank(),
              panel.border=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_rect(fill = "grey96", color = "grey96"), title = element_text(size = 28)) +
        geom_segment(aes(x = 90, y = 147, xend = 90, yend = 194)) + geom_segment(aes(x = 108, y = 147, xend = 108, yend = 194)) +
        geom_segment(aes(x = 126, y = 147, xend = 126, yend = 194)) + geom_segment(aes(x = 143, y = 147, xend = 143, yend = 194)) + 
        geom_segment(aes(x = 90, y = 147, xend = 143, yend = 147)) + geom_segment(aes(x = 90, y = 163, xend = 143, yend = 163)) + 
        geom_segment(aes(x = 90, y = 179, xend = 143, yend = 179)) + geom_segment(aes(x = 90, y = 194, xend = 143, yend = 194)) +
        geom_segment(aes(x = max(coordinates$x), y = min(coordinates$y), xend = max(coordinates$x), yend = max(coordinates$y))) + geom_segment(aes(x = min(coordinates$x), y = min(coordinates$y), xend = min(coordinates$x), yend = max(coordinates$y))) + 
        geom_segment(aes(x = min(coordinates$x), y = max(coordinates$y), xend = max(coordinates$x), yend = max(coordinates$y))) + geom_segment(aes(x = min(coordinates$x), y = min(coordinates$y), xend = max(coordinates$x), yend = min(coordinates$y))) +
        geom_point(aes(x = click_point_x$data, y = click_point_y$data), color = "red", size = 2.5) + geom_polygon(aes(x = x, y = y), data = df, color = "red", fill = NA)
    }
  }

  ########################################################  
  
  
  ## Plot output
  
  output$pitch_plot <- renderPlot({
    list.pitch <- the.big.guy.pitcher.radii(input$pitcher_name, input$other_pitcher, "R")
    list.pitch[[1]]
  })
  
  output$pitch_plot_2 <- renderPlot({
    list.pitch <- the.big.guy.pitcher.radii(input$pitcher_name, input$other_pitcher, "L")
    list.pitch[[1]]
  })

  output$pitch_plot_3 <- renderPlot({
    strike.zone.plot()
  })
  
  output$plot_near_points <- renderTable({
    res <- nearPoints(coordinates, input$plot_click, "x", "y", maxpoints = 1)
  })
  
  output$accuracy <- renderPrint({
    if (input$other_pitcher == "None"){
      list.pitch <- the.big.guy.pitcher.radii(input$pitcher_name, input$other_pitcher, "L")
      str1 <- paste("X Coordinate: ", as.character(click_point_x$data), sep = "")
      str2 <- paste("Y Coordinate: ", as.character(click_point_y$data), sep = "")
      str3 <- paste("K-Value for kknn Model: ", as.character(list.pitch[[2]]), sep ="")
      str4 <- paste("Average Euclidean Distance from Pitch: ", as.character(list.pitch[[3]]), sep ="")
      str5 <- paste("Pitcher Name: ", as.character(input$pitcher_name), sep = "")
      str6 <- paste("Pitcher Comparison Name: ", as.character(input$other_pitcher), sep ="")
      cat(paste(str5, str6, sep = '\n'))
      cat('\n')
      cat('\n')
      cat(paste(str1, str2, sep = '\n'))
      cat('\n')
      cat('\n')
      cat(paste(str3, str4, sep = '\n'))

    }
    else{
      list.pitch <- the.big.guy.pitcher.radii(input$pitcher_name, input$other_pitcher, "L")
      str1 <- paste("X Coordinate: ", as.character(click_point_x$data), sep = "")
      str2 <- paste("Y Coordinate: ", as.character(click_point_y$data), sep = "")
      str3 <- paste("K-Value for kknn Model for First Pitcher: ", as.character(list.pitch[[2]]), sep ="")
      str4 <- paste("K-Value for kknn Model for Other Pitcher: ", as.character(list.pitch[[3]]), sep = "")
      str7 <- paste("Average Euclidean Distance from Pitch 1: ", as.character(list.pitch[[4]]), sep ="")
      str8 <- paste("Average Euclidean Distance from Pitch 2: ", as.character(list.pitch[[5]]), sep ="")
      str5 <- paste("Pitcher Name: ", as.character(input$pitcher_name), sep = "")
      str6 <- paste("Pitcher Comparison Name: ", as.character(input$other_pitcher), sep ="")
      cat(paste(str5, str6, sep = '\n'))
      cat('\n')
      cat('\n')
      cat(paste(str1, str2, sep = '\n'))
      cat('\n')
      cat('\n')
      cat(paste(str3, str4, sep = '\n'))
      cat('\n')
      cat('\n')
      cat(paste(str7, str8, sep = '\n'))
    }
  })
  
})
# Run the application 
shinyApp(ui = ui, server = server)
