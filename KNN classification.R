library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)
library(kknn)
library(reshape2)


# Load data (from pitches and the API)
dat <- scrape(start = "2016-06-18", end = "2016-06-19")

pitchFX <- plyr::join(dat$atbat, dat$pitch, by = c("num", "url"), type = "inner")

duplicates <- duplicated(t(pitchFX))
pitchFX <- pitchFX[,!duplicates]

# Clean data
pitches.clean <- pitchFX %>% dplyr::select(b, s, o, stand, p_throws, event, inning, batter_name, pitcher_name, date, des, x, y, start_speed, end_speed, pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y, break_angle, break_length, pitch_type, zone, nasty, spin_dir, spin_rate, count)

pitches.clean <- season

pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)
pitches.clean$des <- as.character(pitches.clean$des)
duplicates <- duplicated(t(pitches.clean))
pitches.clean <- pitches.clean[,!duplicates]
pitches.clean <- pitches.clean%>% dplyr::filter(pitch_type != "PO", pitch_type != "IN")
pitches.clean <- pitches.clean %>% dplyr::filter(des != "Hit By Pitch", des != "Intent Ball", des != "Missed Bunt", des != "Pitchout", des != "Automatic Ball")
pitches.clean$des <- as.factor(pitches.clean$des)

levels(pitches.clean$des) <- c("Ball", "Ball", "Called Strike", "Foul", "Foul", "Foul", "Foul", "In play, no out", "In play, out", "In play, no out", "Swinging Strike", "Swinging Strike")
pitches.clean$des <- as.factor(pitches.clean$des)

pitches.clean$idNum <- sample(1:nrow(pitches.clean), nrow(pitches.clean))

# Add Outcome Columns
outcome <- pitches.clean  %>%  dplyr::select(event, des, idNum)

rows <- nrow(outcome)
for (i in 1:rows){
  if (outcome[i,2] == "In play, out" | outcome[i,2] == "In play, no out"){
    outcome[i,4] = as.character(outcome[i,1])
  }
  else{
    outcome[i,4] = as.character(outcome[i,2])
  }
}

names(outcome) <- c("event", "des", "idNum", "end")
pitches.clean <- pitches.clean %>% dplyr::select(-event, -des)
pitches.outcomes <- merge(pitches.clean, outcome, by = "idNum")
pitches.outcomes <- pitches.outcomes %>% filter(end != "Bunt Groundout", end != "Bunt Pop Out", end != "Double Play", end != "Field Error", end != "Sac Bunt", end != "Batter Interference", end != "Catcher Interference", end != "Bunt Lineout", end != "Fan interference")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)

# THIS LINE NEEDS TO BE FIXED
levels(pitches.outcomes$end) <- c("Ball", "Called Strike", "XB Hit", "Groundout", "Groundout", "Fly Out", "Groundout","Foul","Groundout","Groundout","Home Run","Lineout","Pop Out","Fly Out","Fly Out", "Single","Swinging Strike","XB Hit")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)

# Select our relevant columns, split into train and test

# Season2016Final data frame goes up to here, so start running the code here.
load("season2016Final.RData")
pitches.outcomes$pitch_type <- as.character(pitches.outcomes$pitch_type)
pitches.outcomes <- pitches.outcomes %>% dplyr::filter(pitch_type != "IN", pitch_type != "PO", pitch_type != "UN", pitch_type != "SC", pitch_type != "AB", pitch_type != "FO")
pitches.outcomes$pitch_type <- as.factor(pitches.outcomes$pitch_type)
levels(pitches.outcomes$pitch_type) <- c("CH","CU","EP","FC","FF","SI","FT","KC","KN","SI","SL")
pitches.clean <- pitches.outcomes

pitches.model.data <- pitches.outcomes %>% dplyr::select(start_speed, break_length, spin_rate, pfx_z, zone, stand, pitcher_name, end)
pitches.model.data <- na.omit(pitches.model.data)

scale.train.object <- preProcess(pitches.model.data[,1:4])
pitches.model.data[,1:4] <- scale(pitches.model.data[,1:4])
pitches.model.data <- na.omit(pitches.model.data)




#### ONLY RUN UP TO THIS POINT TO RUN SCRIPT



index <- createDataPartition(y = pitches.model.data$end, p=0.8)[[1]]
pitches.train <- pitches.model.data[index,]
pitches.test <- pitches.model.data[-index,]

pitches.train <- na.omit(pitches.train)
pitches.test <- na.omit(pitches.test)

# Use knn to determine the optimal number of k-neighbors to choose (14 is our best accuracy)
library(class)
accuracy <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction <- knn(pitches.train[,-c(13,14)], pitches.test[,-c(13,14)], (pitches.train$end), k = x)
  accuracy[x] <- mean(prediction == pitches.test$end)
}
plot(k, accuracy, type = 'b')

# Use kknn to make a model
kknn.all <- kknn(pitches.train$end ~ ., train = filter(pitches.train)[-c(13,14)], test = pitches.test[-c(13,14)], k = 14)
kknn.all$prob

# Now write a function to generate barplot for a specific zone, pitch
attempt.pitch <- pitches.clean[450,]

# Regular the.big.guy for right hitters
the.big.guy.R <- function(zone_id) {
  model <- kknn(filter(pitches.model.data, zone == zone_id, stand == "R")$end ~ ., train = filter(pitches.model.data, zone == zone_id, stand == "R")[-c(5:8)], test = attempt.pitch, k = 14)
  m2 <- data.frame(model$prob)
  outcomes <- melt(m2)
  outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
  ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
}

# Heres the.big.guy for right hitters, allowing you to pass a pitcher name
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
  model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:8)], test = attempt.pitch, k = sqrt(nrow(relevant.data)))
  m2 <- data.frame(model$prob)
  outcomes <- melt(m2)
  outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
  ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
}

# Here.s the big guy left with functionality for comparisons 
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


