library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)

data("pitches")

dat <- scrape(start = "2012-06-03", end = "2012-06-03")
pitchFX <- plyr::join(dat$atbat, dat$pitch, by = c("num", "url"), type = "inner")

pitches.clean <- pitchFX
pitches.clean$idNum <- sample(1:nrow(pitches.clean), nrow(pitches.clean))
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
levels(pitches.outcomes$end) <- c("Ball", "Called Strike", "Double", "Groundout", "Flyout", "Groundout","Foul","Groundout","Groundout","Home Run","Lineout","Pop Out","Sac Fly","Single","Swinging Strike","Triple")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)

pitches.clean <- pitches.outcomes

pitches.model.data <- pitches.clean %>% select(start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate, zone, end)

pitches.model.data <- na.omit(pitches.model.data)

pitches.model.data[,1:12] <- scale(pitches.model.data[,1:12])

index <- createDataPartition(y = pitches.model.data$end, p=0.8)[[1]]
pitches.train <- pitches.model.data[index,]
pitches.test <- pitches.model.data[-index,]

pitches.train <- na.omit(pitches.train)
pitches.test <- na.omit(pitches.test)

# Use for kknn
kknn.all <- kknn(pitches.train$end ~ ., train = filter(pitches.train)[-c(13,14)], test = pitches.test[-c(13,14)], k = 18)

kknn.all$prob

kknn.zone.4 <- kknn(filter(pitches.train, zone == 4)$end ~ ., train = filter(pitches.train, zone == 4)[-c(13,14)], test = filter(pitches.test, zone ==4)[-c(13,14)], k = 18)

kknn.zone.4$prob

zones <- c(1:9, 11:14)
for(x in zones) {
  assign(paste("kknn.zone.", x, sep=""), kknn(filter(pitches.train, zone == 4)$end ~ ., train = filter(pitches.train, zone == 4)[-c(13,14)], test = filter(pitches.test, zone ==4)[-c(13,14)], k = 18))
}

kknn.zone.2


## Old Method using kkn (no probabilites)
# kth nearest neighbor on pitch outcomes
pitches.model.data <- pitches.clean %>% select(start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate, zone, des)

pitches.model.data <- na.omit(pitches.model.data)

pitches.model.data[,1:12] <- scale(pitches.model.data[,1:12])

index <- createDataPartition(y = pitches.model.data$des, p=0.8)[[1]]
pitches.train <- pitches.model.data[index,]
pitches.test <- pitches.model.data[-index,]

pitches.train <- na.omit(pitches.train)
pitches.test <- na.omit(pitches.test)

library(class)
knn.1 <- knn(pitches.train[,-c(13,14)], pitches.test[,c(-13,-14)], (pitches.train$des), k = 11)

# 18 is our best accuracy
accuracy <- rep(0, 20)
k <- 1:20
for(x in k){
  prediction <- knn(pitches.train[,-c(13,14)], pitches.test[,-c(13,14)], (pitches.train$des), k = x)
  accuracy[x] <- mean(prediction == pitches.test$des)
}

plot(k, accuracy, type = 'b')

knn.2 <- knn(pitches.train[,-c(13,14)], pitches.test[-c(13,14)], (pitches.train$des), k = 18)

table(knn.2, pitches.test$des)
barplot(prop.table(table(knn.2, pitches.test$des), 1)[1,])

outcomes_list <- list()
zones <- c(1:9, 11:14)
for(x in zones) {
  outcomes_list[[x]] <- knn(filter(pitches.train, zone == x)[-c(13,14)], filter(pitches.test, zone == x)[-c(13,14)], filter(pitches.train, zone == x)$des, k = 14)
}



