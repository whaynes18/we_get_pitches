the.big.guy.pitcher.radii <- function(pitcher = "All", other_pitcher = "None", stance, x_val = 150, y_val = 250) {
  
  if (stance == "R"){
    title = "Comparison Pitch Outcome Distribution - Righty Hitters"
  }
  else{
    title = "Comparison Pitch Outcome Distribution - Lefty Hitters"
  }
  
  if (pitcher != "All") {
    if (other_pitcher != "None") {
      if (other_pitcher == "All"){
        relevant.data <- filter(pitches.model.data, pitcher_name == pitcher, stand == stance)
        relevant.data.2 <- filter(pitches.model.data, pitcher_name != pitcher, stand == stance)
      }
      else{
        relevant.data <- filter(pitches.model.data, pitcher_name == pitcher, stand == stance)
        relevant.data.2 <- filter(pitches.model.data, pitcher_name == other_pitcher, stand == stance)
      }
      
      n.col <- length(relevant.data)
      for (i in 1:nrow(relevant.data)){
        relevant.data[i, n.col + 1] = sqrt((relevant.data[i,5] - x_val)^2 + (relevant.data[i,6] - y_val)^2)
      }
      relevant.data = relevant.data %>% dplyr::filter(relevant.data[,n.col + 1] < 8)
      
      model.pitcher <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:10)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
      model.all <- kknn(relevant.data.2$end ~ ., train = relevant.data.2[-c(5:10)], test = test.pitch2(), k = sqrt(nrow(relevant.data.2)))
      
      pitcher.probs <- as.numeric(model.pitcher$prob)
      all.probs <- as.numeric(model.all$prob)
      outcomeLevels <- levels(pitches.clean$end)
      both.probs <- data.frame(outcomeLevels, pitcher.probs, all.probs)
      
      outcomes <- melt(both.probs)
      all <- outcomes %>% filter(variable == "all.probs")
      selected.pitcher <- outcomes %>% filter(variable == "pitcher.probs")
      
      selected.pitcher$other <- selected.pitcher$outcomeLevels
      all$other <- "League Average"
      outcomes <- rbind(selected.pitcher, all)
      
      outcomes$outcomeLevels <- factor(all$outcomeLevels,levels(all$outcomeLevels)[c(10, 2, 4, 1, 5, 8, 3, 7, 9, 11, 6)])
      selected.pitcher$outcomeLevels <- factor(selected.pitcher$outcomeLevels,levels(selected.pitcher$outcomeLevels)[c(10, 2, 4, 1, 5, 8, 3, 7, 9, 11, 6)])
      
      ggplot(outcomes, aes(x = outcomeLevels, y = value, fill = other)) + theme_bw() + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) + 
        geom_bar(colour = "black", stat = "identity", position = "dodge") + scale_fill_manual(values = c("yellow1", "green3", "red3", "green3", "blue3", "red3", "blue3", "blue3", "red3", "green3", "red3", "grey75")) + 
        ylab("Probability") + xlab("") + ggtitle(title) + guides(fill = FALSE)
    }
    else{
      
      relevant.data <- filter(pitches.model.data, pitcher_name == pitcher, stand == stance)
      
      n.col <- length(relevant.data)
      for (i in 1:nrow(relevant.data)){
        relevant.data[i, n.col + 1] = sqrt((relevant.data[i,5] - x_val)^2 + (relevant.data[i,6] - y_val)^2)
      }
      relevant.data = relevant.data %>% dplyr::filter(relevant.data[,n.col + 1] < 8)
      
      model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:10)], test = pitch.test, k = sqrt(nrow(relevant.data)))
      m2 <- data.frame(model$prob)
      outcomes <- melt(m2)
      outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
      
      ggplot(outcomes, aes(x = variable, y = value)) + theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) + 
        guides(fill = FALSE) + scale_fill_manual(values = c("green3", "green3", "green3","yellow1", "blue3", "blue3", "blue3", "blue3","red3", "red3", "red3")) + 
        geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("") + ggtitle(title)
    }
  } 
  else {
    
    relevant.data <- filter(pitches.model.data, stand == stance)
    
    n.col <- length(relevant.data)
    for (i in 1:nrow(relevant.data)){
      relevant.data[i, n.col + 1] = sqrt((relevant.data[i,5] - x_val)^2 + (relevant.data[i,6] - y_val)^2)
    }
    relevant.data = relevant.data %>% dplyr::filter(relevant.data[,n.col + 1] < 8)
    
    model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:10)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
    m2 <- data.frame(model$prob)
    outcomes <- melt(m2)
    outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
    
    ggplot(outcomes, aes(x = variable, y = value)) + theme_bw()+ theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("green3", "green3", "green3","yellow1", "blue3", "blue3", "blue3", "blue3","red3", "red3", "red3")) + 
      geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("") + ggtitle(title)
  }
}
