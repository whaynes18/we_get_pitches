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

