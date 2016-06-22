library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)
library(kknn)

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
levels(pitches.outcomes$end) <- c("Ball", "Called Strike", "Double", "Groundout", "Flyout", "Groundout","Foul","Groundout","Groundout","Home Run","Lineout","Pop Out","Sac Fly","Sac Fly", "Single","Swinging Strike","Triple")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)

# Select our relevant columns, split into train and test
pitches.clean <- pitches.outcomes

pitches.test.features <- pitches.clean  %>% select(start_speed, end_speed, sz_top, sz_bot, pfx_x, pfx_z, break_angle, break_length, break_y, spin_dir, spin_rate, end)

TRAINCONTROL <- trainControl(method = "cv", repeats = 3, verboseIter = F, summaryFunction = multiClassSummary)

# scale the data
ends <- pitches.test.features$end
pitches.test.features2 <- scale(pitches.test.features[,1:10])
pitches.test.features2 <- data.frame(pitches.test.features2)
pitches.test.features2$end <- ends

pitch.rpart <- train(end ~ ., data = pitches.test.features, method = "rpart", control = rpart.control(minsplit = 10, minbucket = 3), trControl = TRAINCONTROL)
varImp(pitch.rpart)

# The four most important factors are pfx_z, break_length, start_speed, spin_rate).
# Now I am going to see if it is different for individual zones

features.zone <- pitches.clean  %>% select(start_speed, end_speed, sz_top, sz_bot, pfx_x, pfx_z, break_angle, break_length, break_y, spin_dir, spin_rate, px, pz, end, zone)

ends <- features.zone$end
zones <- features.zone$zone
features.zone2 <- scale(features.zone[,1:12])
features.zone2 <- data.frame(features.zone2)
features.zone2$end <- ends
features.zone2$zone <- zones

features.zone2$end <- as.character(features.zone2$end)
zone1 <- features.zone2 %>% filter(zones == 6)
zone1 <- zone1 %>% select(-zone)
zone1$end <- as.factor(zone1$end)

pitch.rpart <- train(end ~ ., data = zone1, method = "rpart", control = rpart.control(minsplit = 10, minbucket = 3), trControl = TRAINCONTROL)
varImp(pitch.rpart)

# Interestingly, some of the important features are different depending on the zone. But, we 
# suspect this is just from small amounts of data. In a more important finding, we see that 
# location does not matter within zones. By this, I mean that px and pz have O importance in
# the varImp table while in zones. But, for the overall model, pz and px rank as the most important.
# This just means that the overall location matters (as expected) but that the position within
# a zone does not affect the outcome significantly.

# Figure out the mean values of the four features for different types of pitches so that
# we can have default values for our sliders

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

