library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)
library(kknn)

# Load data (from pitches and the API)
dat <- scrape(start = "2016-04-01", end = "2016-04-30")
pitchFX <- plyr::join(dat$atbat, dat$pitch, by = c("num", "url"), type = "inner")

# Clean data
pitches.clean <- season
pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)
duplicates <- duplicated(t(pitches.clean))
pitches.clean <- pitches.clean[,!duplicates]
pitches.clean <- pitches.clean%>% filter(pitch_type != "PO", pitch_type != "IN")
pitches.clean <- pitches.clean %>% filter(des != "Intent Ball", des != "Missed Bunt", des != "Pitchout", des != "Automatic Ball")
pitches.clean$des <- as.factor(pitches.clean$des)
levels(pitches.clean$des) <- c("Ball", "Ball", "Called Strike", "Foul", "Foul", "Foul", "Foul", "Ball", "In play, no out", "In play, out", "In play, no out", "Swinging Strike", "Swinging Strike")
pitches.clean$des <- as.factor(pitches.clean$des)

pitches.clean$idNum <- sample(1:nrow(pitches.clean), nrow(pitches.clean))


# Add Outcome Columns
outcome <- pitches.clean  %>%  select(event, des, idNum)

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

pitcher.find <- function(pitcher){
  (pitcher)
  pitches.clean$match <- str_count(pitcher, pitches.clean$pitcher_name) 
  pitches.clean %>% filter(match == 1)
}

hurlers <- pitches.clean %>% dplyr::select(pitcher_name, pitch_type, start_speed, break_length, pfx_z, spin_rate)
hurlers$pitcher_name <- as.factor(hurlers$pitcher_name)
hurlers$pitch_type <- as.factor(hurlers$pitch_type) 

# this creates a label for each pitch which combines the pitchers name and the pitch type
hurlers$name_and_pitch <- with(hurlers, interaction(pitcher_name, pitch_type))

# gives an array with the mean speed of each pitch for each pitcher
speed_mean <- with(hurlers, tapply(start_speed, name_and_pitch, mean)) %>% adply(1)
break_length_mean <- with(hurlers, tapply(break_length, name_and_pitch, mean)) %>% adply(1)
pfx_z_mean <- with(hurlers, tapply(pfx_z, name_and_pitch, mean)) %>% adply(1)
spin_rate_mean <- with(hurlers, tapply(spin_rate, name_and_pitch, mean)) %>% adply(1)

names(speed_mean) <- c("Pitcher and Type", "Average Start Speed")
names(break_length_mean) <- c("Pitcher and Type", "Average Break Length")
names(pfx_z_mean) <- c("Pitcher and Type", "Average pfx_z")
names(spin_rate_mean) <- c("Pitcher and Type", "Average Spin Rate")

final <- merge(speed_mean, break_length_mean, by = "Pitcher and Type")
final <- merge(final, pfx_z_mean, by = "Pitcher and Type")
final <- merge(final, spin_rate_mean, by = "Pitcher and Type")

final$`Pitcher and Type` <- as.character(final$`Pitcher and Type`)
final$Pitcher <- lapply(final$`Pitcher and Type`, function(n) {str_split(n, '\\.')[[1]][1]})
final$Pitch_type <- lapply(final$`Pitcher and Type`, function(n) {str_split(n, '\\.')[[1]][2]})

# We might want to omit the NA's. Na's represent pitches that the pitcher does not have. We
# don't know if we want to do this yet. NA's could be helpful
# final <- na.omit(final)


  
  
