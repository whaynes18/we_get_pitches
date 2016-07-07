library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)
library(kknn)

# Load data (from pitches and the API)
dat <- scrape(start = "2016-04-01", end = "2016-04-03")
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

pitches.test.features <- pitches.clean  %>% select(start_speed, end_speed, pfx_x, pfx_z, break_angle, break_length, break_y, spin_dir, spin_rate, end)
pitches.test.features.all <- pitches.clean  %>% select(x, y, start_speed, end_speed, pfx_x, pfx_z, break_angle, break_length, break_y, spin_dir, spin_rate, vx0, vy0, vz0, ax, ay, az, x0, y0, z0, px, pz, end)

pitches.test.features.noPos <- pitches.clean  %>% select(start_speed, end_speed, pfx_x, pfx_z, break_angle, break_length, break_y, spin_dir, spin_rate, vx0, vy0, vz0, ax, ay, az, end)

TRAINCONTROL <- trainControl(method = "cv", repeats = 3, verboseIter = F, summaryFunction = multiClassSummary)

# scale the data
ends <- pitches.test.features.all$end
pitches.test.features2 <- scale(pitches.test.features.all[,1:(length(pitches.test.features.all) - 1)])
pitches.test.features2 <- data.frame(pitches.test.features2)
pitches.test.features2$end <- ends

pitch.rpart <- train(end ~ ., data = pitches.test.features2, method = "rpart", control = rpart.control(cp = 0.03, minsplit = 10, minbucket = 3), trControl = TRAINCONTROL)
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


### FINDING ZONES AND USING X,Y VALUES IN OUR MODEL

pitches.model.data <- pitches.clean %>% dplyr::select(start_speed, break_length, spin_rate, pfx_z, x, y, pitch_type, stand, pitcher_name, end)
pitches.model.data <- na.omit(pitches.model.data)

scale.train.object <- preProcess(pitches.model.data[,1:4])
pitches.model.data[,1:4] <- scale(pitches.model.data[,1:4])
pitches.model.data <- na.omit(pitches.model.data)

pitches.outcomes$zone <- as.factor(pitches.outcomes$zone)
ggplot(pitches.outcomes, aes(x = x, y = y)) + geom_point(aes(color = zone)) + scale_x_continuous(limits = c(50, 200)) + scale_y_continuous(limits = c(90,265)) + scale_color_manual(values = c("blue","yellow","blue","yellow","blue","yellow", "blue","yellow", "blue", "grey52","grey14","grey14","grey52")) 
  geom_vline(xintercept = c(90, 108, 126, 144), color = "red", size = 2) + geom_hline(yintercept = c(195,179,163,147), size = 2, color = "red") + geom_vline(xintercept = 117, size = 1.5, color = "white") + geom_hline(yintercept = 171, size = 1.5, color = "white")

ggplot(pitches.outcomes, aes(x = px, y = pz)) + geom_point(aes(color = zone)) + scale_x_continuous(limits = c(-1, 1)) + scale_y_continuous(limits = c(1,4.5)) + scale_color_manual(values = c("blue","yellow","blue","yellow","blue","yellow", "blue","yellow", "blue", "grey52","grey14","grey14","grey52"))+ 
  geom_vline(xintercept = c(-.7, .7), color = "red", size = 2) + geom_hline(yintercept = c(1.6, 3.6), size = 2, color = "red") + geom_vline(xintercept = 0, size = 1.5, color = "white") + geom_hline(yintercept = 2.5, size = 1.5, color = "white")


# this graph shows me that the center of the strike zone is at the coordinates (117,171). Now i will make radial coordiantes based on this center
# so, this means that a strike zone is 18 units wide and 16 units high. I am going to use this estimation
# to CREATE THE RADIUS FOR THE CIRCLE THAT WILL CLOSE OFF DATA for the further model. I am going to 
# make the radius equal to 16. This will capture all the height in an artificial zone.


ggplot(pitches.outcomes, aes(x = px, y = pz)) + geom_point(aes(color = zone)) + scale_x_continuous(limits = c(-1, 1)) + scale_y_continuous(limits = c(1,4.5)) + scale_color_manual(values = c("blue","yellow","blue","yellow","blue","yellow", "blue","yellow", "blue", "grey52","grey14","grey14","grey52"))+ 
  geom_vline(xintercept = c(-.7, (-0.7 + (2 * .7 / 3)), .7), color = "red", size = 2) + geom_hline(yintercept = c(1.5, (2.5 + 1/3), 3.5), size = 2, color = "red") + geom_vline(xintercept = 0, size = 1.5, color = "white") + geom_hline(yintercept = 2.5, size = 1.5, color = "white")

# we should actually use px and pz rather than x and y.... so now, the center is roughly at (0, 2.5), and
# the bounds are at (-.7, .7) and (1.5, 3.5). Furthermore, the height of one zone is roughly (3.5 - (2.5 + 1/3)) = 2/3.
# So, we want the radius of our circle to be (2/3)/2 = 2/6 = 1/3.
# OKAY, SO I DON'T WANT TO CHANGE EVERYTHING NOW, BETTER TO JUST MAKE THE MARKDOWN, BUT CHANGE
# THIS IN THE FUTURE

pitches.outcomes$radius = sqrt((pitches.outcomes$x - 117)^2 + (pitches.outcomes$y - 171)^2)
tail(sort(pitches.outcomes$radius), n = 30)

# filter out the weird super outlier pitches
pitches.outcomes2 <- pitches.outcomes %>% filter(radius < 160)

# plot that shows the radius and the strike zone (red lines)
ggplot(pitches.outcomes2, aes(x = x, y = y)) + scale_x_continuous(limits = c(78, 156)) + scale_y_continuous(limits = c(127,215)) +  geom_point(aes(color = radius)) + scale_color_gradient(low = "white", high = "grey7", limits = c(0, 60)) + 
  geom_vline(xintercept = c(90, 144), color = "red") + geom_hline(yintercept = c(195, 147), color = "red") # + geom_vline(xintercept = 117, size = 1.5, color = "white") + geom_hline(yintercept = 171, size = 1.5, color = "white")

pitch.example <- pitches.outcomes[16,]  
  
create_radii <- function(x_value, y_value){
  pitches.model.data$radius <- sqrt((pitches.model.data$x - x_value) ^ 2 + (pitches.model.data$y - y_value) ^ 2)
  pitches.model.data
}

# this makes the given point the center of the graph. must save it into a new graph though
pop <- create_radii(pitch.example)

ggplot(pop, aes(x = x, y = y)) + scale_x_continuous(limits = c(78, 156)) + scale_y_continuous(limits = c(127,215)) +  geom_point(aes(color = radius)) + scale_color_gradient(low = "white", high = "grey7", limits = c(0, 8)) + 
  geom_vline(xintercept = c(90, 144), color = "red") + geom_hline(yintercept = c(195, 147), color = "red") + geom_vline(xintercept = 117, size = 1.5, color = "white") + geom_hline(yintercept = 171, size = 1.5, color = "white")

# nice ^

# make pitches either in the strikeZone or not in the strikeZone

finalColName <- paste("V", (ncols + 1), sep = "")
pitches.clean <- pitches.clean  %>% dplyr::rename(strikeZone = finalColName)
pitches.clean$strikeZone <- as.factor(pitches.clean$strikeZone)

# plot the stike zone essentially
ggplot(pitches.clean, aes(x=x, y=y)) + geom_point(aes(color = strikeZone)) + scale_color_manual(values = c("black", "white"))+ scale_x_continuous(limits = c(78, 156)) + 
  scale_y_continuous(limits = c(127,215))
# so, this gives us all the points in the data frame, and gives us a pretty accurate strike zone. But, it would not
# look very clean with the corners missing points and what not. So, I will make a data frame that has every possible point
# in a certain range, then assign these points to strike zones. This should be cleaner. Also going to be huge, so
# lets see if its even worth it.

x_vals <- seq(78,156,1)
y_vals <- seq(127,215,1)
y_vals <- rep(y_vals, each = length(x_vals))

coordinates <- data.frame(y_vals, x_vals)
ncols <- length(coordinates)
for (i in 1:nrow(coordinates)){
  if ((coordinates[i,2] < 144) & (coordinates[i,2] > 90) & (coordinates[i,1] > 147) & (coordinates[i,1] < 195)){
    coordinates[i, ncols + 1] = 1
  }
  else{
    coordinates[i, ncols + 1] = 0
  }
}
finalColName <- paste("V", (ncols + 1), sep = "")
coordinates <- coordinates  %>% dplyr::rename(strikeZone = V3)
coordinates$strikeZone <- as.factor(coordinates$strikeZone)
ggplot(coordinates, aes(x = x_vals, y = y_vals)) + geom_point(aes(color = strikeZone)) + scale_color_manual(values = c("black", "white"))


# also, do more exploring. Earlier in this file, i tried to see if location within a zone
# matttered for the pitch, and it did not. but, we can check this again. see if location within
# the radial zone affects the outcome of the pitch.
# note: make a new data frame with a list of pitchers who have more than n amount of pitches.
# use this for the drop down list of pitchers. do not use this for the actual data, because
# we still want all the pitches possible to be used in our model.

# create a data frame with the pitcher name and whether or not they have thrown 300 pitches
relevant.pitchers <- ((table(pitches.outcomes$pitcher_name)) > 300) 
relevant.pitchers<- adply(relevant.pitchers, 1)
names(relevant.pitchers) <- c("Pitcher_name", "Relevancy")
relevant <- relevant.pitchers[,2]
# this is a list of all the pitchers who have pitched more than 300 pitches
relevant.names <- sort(as.character(relevant.pitchers[relevant, 1]))




# try the big guy using radius, rather than zone.
the.big.guy.pitcher.radii <- function(pitcher = "All", other_pitcher = "None", stance, pitch) {
  
  # create radius values from the given pitch (its x and y values), then filter to only values
  # with a radius of less than 8
  pitches.model.data <- create_radii(pitch) %>% dplyr::filter(radius < 8)
  
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
      
      model.pitcher <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:10)], test = pitch, k = sqrt(nrow(relevant.data)))
      model.all <- kknn(relevant.data.2$end ~ ., train = relevant.data.2[-c(5:10)], test = pitch, k = sqrt(nrow(relevant.data.2)))
      
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
      model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:10)], test = pitch, k = sqrt(nrow(relevant.data)))
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
    model <- kknn(relevant.data$end ~ ., train = relevant.data[-c(5:10)], test = pitch, k = sqrt(nrow(relevant.data)))
    m2 <- data.frame(model$prob)
    outcomes <- melt(m2)
    outcomes$variable <- factor(outcomes$variable,levels(outcomes$variable)[c(11, 2, 6,1, 4, 9, 5, 8, 10, 3, 7)])
    
    ggplot(outcomes, aes(x = variable, y = value)) + theme_bw()+ theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("green3", "green3", "green3","yellow1", "blue3", "blue3", "blue3", "blue3","red3", "red3", "red3")) + 
      geom_bar(stat = "identity", colour = "black", aes(fill = variable)) + ylab("Probability") + xlab("") + ggtitle(title)
  }
}



  
  
  
  
    
