library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)
library(caret)
library(kknn)
library(reshape2)


######################################################################

# DATA - ORIGIN, COLLECTION, AND CLEANING

######################################################################

# Scrape this year's data, starting with April 01, 2016
dat <- scrape(start = "2016-04-01", end = "2016-06-18")

# Create a dataframe with all the relevant information we want, based off the pitchRx scrape
pitchFX <- plyr::join(dat$atbat, dat$pitch, by = c("num", "url"), type = "inner")

# Due to the join, there are some duplicated columns, so delete these.
duplicates <- duplicated(t(pitchFX))
pitchFX <- pitchFX[,!duplicates]

# Create a dataframe with only necessary variables. This deletes some of the variables like "Gameday URL"
# and description in spanish and whatnot.
pitches.clean <- pitchFX %>% dplyr::select(b, s, o, stand, p_throws, event, inning, batter_name, pitcher_name, date, des, x, y, start_speed, end_speed, pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y, break_angle, break_length, pitch_type, zone, nasty, spin_dir, spin_rate, count)

# Alter the types of some of our variables.
pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)
pitches.clean$des <- as.character(pitches.clean$des)

# Filter out certain unnecessary pitches that do not help our model. Then create a set amount
# of factors for the decisions. Many of the decisions can go together in certain categories.
pitches.clean <- pitches.clean%>% dplyr::filter(pitch_type != "PO", pitch_type != "IN")
pitches.clean <- pitches.clean %>% dplyr::filter(des != "Hit By Pitch", des != "Intent Ball", des != "Missed Bunt", des != "Pitchout", des != "Automatic Ball")
pitches.clean$des <- as.factor(pitches.clean$des)
levels(pitches.clean$des) <- c("Ball", "Ball", "Called Strike", "Foul", "Foul", "Foul", "Foul", "In play, no out", "In play, out", "In play, no out", "Swinging Strike", "Swinging Strike")
pitches.clean$des <- as.factor(pitches.clean$des)

# Add id numbers (for merging later)
pitches.clean$idNum <- sample(1:nrow(pitches.clean), nrow(pitches.clean))

# Create a new dataframe that will be used to hold the outcomes of every pitch. The outcome of every
# pitch is based on the outcome of the given pitch and the outcome of the at bat. The essentials
# of this change is that it alters "In play, out" (or no out) to "single" or "lineout" or whatever
# it is. We then merge these outcomes back into the main data frame and filter out a few outcomes
# that will not be relevant to our data.
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

# Concatenate the levels of the outcomes into these categories. This is somewhat hardcoded because
# we know what levels are in the data frame now, and they may change if we load more data, so
# PROCEED WITH CAUTION.
levels(pitches.outcomes$end) <- c("Ball", "Called Strike", "XB Hit", "Groundout", "Groundout", "Fly Out", "Groundout","Foul","Groundout","Groundout","Home Run","Lineout","Pop Out","Fly Out","Fly Out", "Single","Swinging Strike","XB Hit")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)

# Filter out certain pitches that will not be relevant to our data (pitch out, undefined, etc.). The
# last line of this section just combines FS and SI into one category: SI. This is also hard coded
# and could change with different data so proceed with caution.
pitches.outcomes$pitch_type <- as.character(pitches.outcomes$pitch_type)
pitches.outcomes <- pitches.outcomes %>% dplyr::filter(pitch_type != "IN", pitch_type != "PO", pitch_type != "UN", pitch_type != "SC", pitch_type != "AB", pitch_type != "FO")
pitches.outcomes$pitch_type <- as.factor(pitches.outcomes$pitch_type)
levels(pitches.outcomes$pitch_type) <- c("CH","CU","EP","FC","FF","SI","FT","KC","KN","SI","SL")


######################################################################

# EXPLORATORY ANALYSIS

######################################################################

# ---------------------------------------------
# Finding the Important Variables for our Model
# ---------------------------------------------
pitches.clean <- pitches.outcomes

# We want to run a model that eventually decides which pitch characteristics are most important in
# predicting the outcome of the pitch. For this, we will create a data frame with all the possible
# features. Essentially, just take all the numeric features.
pitches.test.features.all <- pitches.clean  %>% select(x, y, start_speed, end_speed, pfx_x, pfx_z, break_angle, break_length, break_y, spin_dir, spin_rate, vx0, vy0, vz0, ax, ay, az, x0, z0, px, pz, end)

TRAINCONTROL <- trainControl(method = "cv", repeats = 3, verboseIter = F, summaryFunction = multiClassSummary)

# Scale the data.
ends <- pitches.test.features.all$end
pitches.test.features2 <- scale(pitches.test.features.all[,1:(length(pitches.test.features.all) - 1)])
pitches.test.features2 <- data.frame(pitches.test.features2)
pitches.test.features2$end <- ends

# Use a decision tree to find the most important variables for predicting outcomes of pitches.
pitch.rpart <- train(end ~ ., data = pitches.test.features2, method = "rpart", control = rpart.control(cp = 0.03, minsplit = 10, minbucket = 3), trControl = TRAINCONTROL)
varImp(pitch.rpart)

# The position variables have the highest importance, but this makes little sense to our model. For
# example, we do not want our outcome distribution model to consider a slider and a fastball too similar,
# even if they are in the same spot. So, we will test the importance again, but this time excluding
# location parameters.

pitches.test.features.noPos <- pitches.test.features2 %>% dplyr::select(-x0, -z0, -px, -pz, -x, -y)
pitch.rpart <- train(end ~ ., data = pitches.test.features.noPos, method = "rpart", control = rpart.control(cp = 0.03, minsplit = 10, minbucket = 3), trControl = TRAINCONTROL)
varImp(pitch.rpart)

# NEED TO TALK ABOUT THIS PART, I CAN'T SEE THE VARIMP NOW DUE TO MY DATA BUT LOOK AT IT AFTER

# ------------------------------------------
# Finding the Boundaries for the Strike Zone
# ------------------------------------------

# Visualize the zones (given by PITCHf/x data) in their x and y coordinates. Then, play around with
# different boundaries and find the rough estimates for the boundaries of the strikezone and
# and different individual zones
pitches.clean$zone <- as.factor(pitches.clean$zone)
ggplot(pitches.clean, aes(x = x, y = y)) + geom_point(aes(color = zone)) + scale_x_continuous(limits = c(50, 200)) + scale_y_continuous(limits = c(90,265)) + scale_color_manual(values = c("blue","yellow","blue","yellow","blue","yellow", "blue","yellow", "blue", "grey52","grey14","grey14","grey52")) + 
  geom_vline(xintercept = c(90, 108, 126, 144), color = "red", size = 2) + geom_hline(yintercept = c(195,179,163,147), size = 2, color = "red") + geom_vline(xintercept = 117, size = 1.5, color = "white") + geom_hline(yintercept = 171, size = 1.5, color = "white")

# Make a data frame with all the possible coordinates (not every single possible necessarily but
# a sufficient amount...) where a pitcher could theoretically pitch. We will create pitches at 
# every single coordinate point. We could add more by switching the step count to 0.5 or whatnot,
# but we found 1 is sufficient. 
x_vals <- seq(78,156,1)
y_vals <- seq(127,215,1)
y_vals <- rep(y_vals, each = length(x_vals))

# Classify each pitch as either in the strikezone or not, based on our estimations from above.
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

# -------------------------------------------
# Create a Vector with only Relevant Pitchers
# -------------------------------------------

# From all the pitchers in our data frame, make a vector (relevant) that holds only pitchers who
# have pitched more than X amount of pitches. We use this for our drop down list in the Shiny App,
# because we do not want to show distributions for pitchers who have a same amount of data.

# WE NEED TO DETERMINE A NON-ARBITRARY X. 

relevant.pitchers <- ((table(pitches.clean$pitcher_name)) > 300) 
relevant.pitchers<- adply(relevant.pitchers, 1)
names(relevant.pitchers) <- c("Pitcher_name", "Relevancy")
relevant <- relevant.pitchers[,2]
relevant.names <- sort(as.character(relevant.pitchers[relevant, 1]))

######################################################################

# MODEL (AND RELEVANT FUNCTIONS)

######################################################################

# create_radii is a function that takes in a selected point's coordinates (x_mid and y_mid) and 
# creates a distance from that point to every point on the coordinate system. x and y take in vectors
# of all the x-coordinates and y-coordinates from the coordinate data frame. x_mid and y_mid will
# be selected using an interactive graph in the Shiny App, but it could also just take in any
# coordinates, if a user has knowledge of the coordinate system.
create_radii <- function(x, y, x_mid, y_mid){
  value <- sqrt((x - x_mid)^2 + (y - y_mid)^2)
  value
}


# calculate_distances calculates the multi-dimensional euclidean distance for each point provided. 
# The input parameters are vectors containing the values for these four variables, which are used
# in our outcome distribution model. This function returns the distance, which will then be
# appended to the data frame. We find this number in order to then average all the distances, which
# can be helpful to estimate an accuracy measure of sorts. 

# NOTE: test.pitch() is a reactive value used in the Shiny App, so it may need to be changed in 
#       order for this function to work outside of the app. See line 592 in app.R to see what test.pitch() is.
calculate_distances <- function(start_speed, pfx_z, break_length, spin_rate){
  dist.df <- data.frame(start_speed, break_length, spin_rate, pfx_z)
  pitch.test <- test.pitch()
  pop <- rbind(dist.df, pitch.test)
  value <- dist(pop)
  value[1]
}


# This function lies within our Shiny App. Thus, it uses certain reactive values that will not
# be applicable in this script. Therefore, we display this function in order to walk through the
# steps and explain what each part means, yet the functionality within this script is minimal.

the.big.guy.pitcher.radii <- function(pitcher = "All", other_pitcher = "None", stance) {
  
  # Extract the x and y coordinates from the chosen point on the interactive plot.
  coords <- coordinate()
  x_val <- coords$x_val
  y_val <- coords$y_val
  
  
  # Create a title for the graph based on the stance of the batter.
  if (stance == "R"){
    title = "Comparison Pitch Outcome Distribution - Righty Hitters"
  }
  else{
    title = "Comparison Pitch Outcome Distribution - Lefty Hitters"
  }
  
  # If pitcher does not equal All, this means a specific pitcher has been selected. If other pitcher
  # does not equal None, then there will be a comparison between the selected pitcher and the other
  # pitcher. If other_pitcher equals All, then compare the first pitcher with all other pitchers in
  # the data set, otherwise just compare the first pitcher with the selected pitcher.
  if (pitcher != "All") {
    if (other_pitcher != "None") {
      if (other_pitcher == "All"){
        # Create two data frames, one with only the pitches from the selected pitcher, and one
        # with the other pitcher(s). 
        relevant.data <- filter(pitches.model.data.2, pitcher_name == pitcher, stand == stance)
        relevant.data.2 <- filter(pitches.model.data.2, pitcher_name != pitcher, stand == stance)
      }
      else{
        relevant.data <- filter(pitches.model.data.2, pitcher_name == pitcher, stand == stance)
        relevant.data.2 <- filter(pitches.model.data.2, pitcher_name == other_pitcher, stand == stance)
      }
      
      # Calculate the radii from the selected point to every other point in the coordinate system.
      # Then, filter the data frame for radii of only less than 8. We chose 8 because this is the
      # estimated height of one of the pre-made zones given by PITCHf/x data. We thought of this
      # to be a good value to take the area of, as it will roughly provide one "zone" around the point.
      # Only do this entire process if the user has selected a point using the interactive plot.
      if (zones$data == 0 & !(is.null(click_point_x$data))){
        relevant.data$radius <- create_radii(relevant.data$x, relevant.data$y, x_val, y_val)
        relevant.data <- relevant.data %>% dplyr::filter(radius < 8)
        
        relevant.data.2$radius <- create_radii(relevant.data.2$x, relevant.data.2$y, x_val, y_val)
        relevant.data.2 <- relevant.data.2 %>% dplyr::filter(radius < 8)
      }
      
      # Create a model using the kknn model function, which is a weighted k-nearest neighbor classifier.
      # Train the data using the four chosen variables (start_speed, pfx_z, break_length, spin_rate), and
      # the outcomes of the pitch. The test pitch is just the values provided by test.pitch(), which
      # takes in user inputs for pitch characterisitcs. This is the part that will not work on a
      # regular script. We set our k-value to the square root of data points we have. This is an
      # estimate and rule-of-thumb number, but will be changed to a more precise number in future updates.
      model.pitcher <- kknn(relevant.data$end ~ ., train = relevant.data[c(1:4, 9)], test = test.pitch(), k = sqrt(nrow(relevant.data)))
      model.all <- kknn(relevant.data.2$end ~ ., train = relevant.data.2[c(1:4, 9)], test = test.pitch2(), k = sqrt(nrow(relevant.data.2)))
      
      # Get the k points that are used in the model. With these points, calculate the euclidean
      # distances for each point to the test pitch, and then average these distances.
      used.points <- model$C
      used <- relevant.data[used.points,]
      used$euc_distances <- calculate_distances(used$start_speed, used$pfx_z, used$break_length, used$spin_rate)
      average_euc <- sum(used$euc_distances) / nrow(used)
      
      used.points.2 <- model$C
      used.2 <- relevant.data[used.points.2,]
      used.2$euc_distances <- calculate_distances(used.2$start_speed, used.2$pfx_z, used.2$break_length, used.2$spin_rate)
      average_euc.2 <- sum(used.2$euc_distances) / nrow(used.2)
      
      # Create a data frame that has all the different outcomes and all the probabilites for both
      # the selected pitcher and the selected other pitcher(s).
      pitcher.probs <- as.numeric(model.pitcher$prob)
      all.probs <- as.numeric(model.all$prob)
      outcomeLevels <- levels(pitches.model.data.2$end)
      both.probs <- data.frame(outcomeLevels, pitcher.probs, all.probs)
      
      # Reshape the data frame so it can be used correctly in the ggplot. The above data frame is
      # more useable for analysis, but does not allow us to create a ggplot that can cleanly
      # show different probabilites between the selected pitchers.
      outcomes <- melt(both.probs)
      all <- outcomes %>% filter(variable == "all.probs")
      selected.pitcher <- outcomes %>% filter(variable == "pitcher.probs")
      selected.pitcher$other <- selected.pitcher$outcomeLevels
      all$other <- "League Average"
      outcomes <- rbind(selected.pitcher, all)
      outcomes$outcomeLevels <- factor(all$outcomeLevels,levels(all$outcomeLevels)[c(10, 2, 4, 1, 5, 8, 3, 7, 9, 11, 6)])
      selected.pitcher$outcomeLevels <- factor(selected.pitcher$outcomeLevels,levels(selected.pitcher$outcomeLevels)[c(10, 2, 4, 1, 5, 8, 3, 7, 9, 11, 6)])

      # Plot a bar graph with the probabilites of each outcome and its comparison
      plot <- ggplot(outcomes, aes(x = outcomeLevels, y = value, fill = other)) + theme_bw() + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(size = 22, face = "bold"), axis.text = element_text(size = 13), axis.title = element_text(size = 19)) + 
        geom_bar(colour = "black", stat = "identity", position = "dodge") + scale_fill_manual(values = c("yellow1", "green3", "red3", "green3", "blue3", "red3", "blue3", "blue3", "red3", "green3", "red3", "grey75")) + 
        ylab("Probability") + xlab("") + ggtitle(title) + guides(fill = FALSE)
      
      # Calculate the k-Value for both models. The k-Value can be used together with the average
      # euclidean distances to render some kind of accuracy. We need to create a metric using these
      # values that makes sense and is readable and understandable to a user.
      k_value <- sqrt(nrow(relevant.data))
      k_value_2 <- sqrt(nrow(relevant.data.2))
      
      # Return the plot, the k-Values, and the average euclidean distances
      return_vals <- list(plot, k_value, k_value_2, average_euc, average_euc.2)
      return_vals
    }
    else{
      
      # Most of the code in here is the same as above with a few minor tweaks to the filters.
      relevant.data <- filter(pitches.model.data.2, pitcher_name == pitcher, stand == stance)
      
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
  
    # Most of the code in here is the same as above with a few minor tweaks to the filters.
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

# This function generates a plot using the coordinate data frame. If a point is clicked on the plot,
# then (in the Shiny App) it uses the chosen point and graphs a circle of radius 8 around that point.
# That circle represents the boundaries for all the possible pitches that can be used in the 
# kknn model. Similarly to the above function, it uses reactive values that are determined in the
# app, so the function does not work on its own in this script.
strike.zone.plot <- function(){
  
  # Extract the x and y coordinates from the chosen point on the interactive plot.  
  coords <- coordinate()
  x_val <- coords$x_val
  y_val <- coords$y_val
  
  # Create a data frame of points that will make up the circle that surrounds the chosen point.
  angle <- seq(-pi, pi, length = 50)
  df <- data.frame(x = x_val + 8*sin(angle), y = y_val + 8*cos(angle))
  
  # Graph the strike zone. If a pitch has yet to be selected, then just graph all the coordinates.
  # Otherwise, graph the strikezone graph with the chosen point and surrounding radius
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







