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

# Clean data
pitches.clean <- pitchFX
pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)
duplicates <- duplicated(t(pitches.clean))
pitches.clean <- pitches.clean[,!duplicates]
pitches.clean <- pitches.clean%>% filter(pitch_type != "PO", pitch_type != "IN")
pitches.clean$des <- as.factor(pitches.clean$des)
levels(pitches.clean$des) <- c("Ball", "Ball", "Called Strike", "Foul", "Foul", "Foul", "Foul", "Ball", "In play, no out", "In play, out", "In play, no out", "Swinging Strike", "Swinging Strike", "Swinging Strike")
pitches.clean$des <- as.factor(pitches.clean$des)

pitches.clean$idNum <- sample(1:nrow(pitches.clean), nrow(pitches.clean))


# Add Outcome Columns
outcome <- pitches.clean  %>%  select(event, des, idNum)

rows <- nrow(outcome)
for (i in 1:rows){
  if (outcome[i,2] == "In play, out" | outcome[i,2] == "In play, no out"){
    outcome[i,4] = outcome[i,1]
  }
  else{
    outcome[i,4] = as.character(outcome[i,2])
  }
}

names(outcome) <- c("event", "des", "idNum", "end")
pitches.outcomes <- merge(pitches.clean, outcome, by = "idNum")
pitches.outcomes <- pitches.outcomes %>% filter(end != "Bunt Groundout", end != "Bunt Pop Out", end != "Double Play", end != "Field Error", end != "Sac Bunt")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)
levels(pitches.outcomes$end) <- c("Ball", "Called Strike", "XB Hit", "Groundout", "Fly Out", "Groundout","Foul","Groundout","Groundout","Home Run","Lineout","Pop Out","Fly Out","Fly Out", "Single","Swinging Strike","XB Hit")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)

# Select our relevant columns, split into train and test
pitches.clean <- pitches.outcomes

pitches.model.data <- pitches.clean %>% select(start_speed, break_length, spin_rate, pfx_z, zone, stand, pitcher_name, end)
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
attempt.zone <- 5
attempt.pitch <- pitches.clean[450,]

the.big.guy <- function(zone_id, pitch) {
  model <- kknn(filter(pitches.model.data, zone == zone_id, stand == "R")$end ~ ., train = filter(pitches.model.data, zone == zone_id, stand == "R")[-c(5:8)], test = pitch, k = 14)
  m2 <- data.frame(model$prob)
  outcomes <- melt(m2)
  outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
  ggplot(outcomes, aes(x = variable, y = value)) + scale_fill_manual(values = c("springgreen3", "springgreen3", "springgreen3","pink1", "sienna1", "sienna1", "sienna1", "sienna1","orangered2", "orangered2", "orangered2")) + geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("Outcome") + ggtitle("Pitch Outcome Distribution")
}