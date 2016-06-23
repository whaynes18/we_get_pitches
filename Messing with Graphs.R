the.big.guy.L.pitcher <- function(zone_id, pitcher = "All", button_pressed = FALSE) {
  if (pitcher != "All") {
    if (button_pressed == TRUE) {
      relevant.data <- filter(pitches.model.data, pitcher_name == pitcher, zone == zone_id, stand == "L")
      relevant.data.2 <- filter(pitches.model.data, zone == zone_id, stand == "L")

      model.pitcher <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = attempt.pitch, k = sqrt(nrow(relevant.data)))
      model.all <- kknn(relevant.data.2$end ~ ., train = relevant.data.2[-c(5:8)], test = attempt.pitch2, k = sqrt(nrow(relevant.data.2)))

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
      
      ggplot(outcomes, aes(x = outcomeLevels, y = value, fill = other)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("pink1", "springgreen3", "orangered2", "springgreen3", "sienna1", "orangered2", "sienna1", "sienna1", "orangered2", "springgreen3", "orangered2", "navajowhite1")) + ylab("Probability") + xlab("Outcome") + ggtitle("Comparison Pitch Outcome Distribution - Lefty Hitters")
      
      
      #ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution - Lefty Hitters")
      
    }
    else {
      relevant.data <- filter(pitches.model.data, pitcher_name == pitcher, zone == zone_id, stand == "L")
      relevant.data <- relevant.data[,-9]
      model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = attempt.pitch, k = sqrt(nrow(relevant.data)))
      m2 <- data.frame(model$prob)
      outcomes <- melt(m2)
      outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
      ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution - Lefty Hitters")
    }
  } else {
    relevant.data <- filter(pitches.model.data, zone == zone_id, stand == "L")
    model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = attempt.pitch, k = sqrt(nrow(relevant.data)))
    m2 <- data.frame(model$prob)
    outcomes <- melt(m2)
    outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
    ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution - Lefty Hitters")
  }
}

zone = 4
pitcher = "All"
attempt.pitch <- pitches.clean[1,]
attempt.pitch2 <- pitches.clean[8,]


### Messing with the guts

relevant.data <- dplyr::filter(pitches.model.data, pitcher_name == "Jon Lester", stand == "R")
relevant.data.2 <- dplyr::filter(pitches.model.data, stand == "R")

model.pitcher <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = attempt.pitch, k = sqrt(nrow(relevant.data)))
model.all <- kknn(relevant.data.2$end ~ ., train = relevant.data.2[-c(5:8)], test = attempt.pitch.2, k = sqrt(nrow(relevant.data.2)))

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

ggplot(outcomes, aes(x = outcomeLevels, y = value, fill = other)) + geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values = c("pink1", "springgreen3", "orangered2", "springgreen3", "sienna1", "orangered2", "sienna1", "sienna1", "orangered2", "springgreen3", "orangered2", "navajowhite1"))


## Stack overflow example
library(ggplot2)
library(reshape)


x = c("Band 1", "Band 2", "Band 3")
y1 = c("1","2","3")
y2 = c("2","3","4")

to_plot <- data.frame(x=x,y1=y1,y2=y2)
melted<-melt(to_plot, id="x")

print(ggplot(melted,aes(x=x,y=value,fill=variable)) + 
        geom_bar(stat="identity",position = "identity", alpha=.3))
class(x)
class(as.character(model.pitcher$prob))
x






