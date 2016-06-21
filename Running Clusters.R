library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)

data("pitches")

dat <- scrape(start = "2012-06-03", end = "2012-06-03")
pitchFX <- plyr::join(dat$atbat, dat$pitch, by = c("num", "url"), type = "inner")

# just for practice here we will do this:
pitches <- pitchFX

ggplot(pitches, aes(x = x, y = y)) + geom_point(aes(color = type))
ggplot(pitches, aes(x = x, y = y)) + geom_point(aes(color = des))

# Clean Pitches Data
pitches.clean <- pitches
pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)

<<<<<<< HEAD

# Cluster the Pitches Data based on characteristics (including location)
duplicates <- duplicated(t(pitches.clean))
pitches.clean2 <- pitches.clean[,!duplicates]
pitches.clean2$idNum <- sample(1:nrow(pitches.clean2), nrow(pitches.clean2))
pitches.clean2 <- filter(pitches.clean2, pitch_type != "PO")
pitches.clean2 <- filter(pitches.clean2, pitch_type != "IN")

# Cluster pitches data on characteristics (without location)
cluster.data.2 <- pitches.clean2 %>% select(idNum, start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate)

cluster.data.2 <- cluster.data.2 %>% select(idNum, start_speed, break_y, break_angle, break_length, spin_dir)

cluster.data.2 <- na.omit(cluster.data.2)
=======
# Cluster pitches data on characteristics (without location)
cluster.data.2 <- pitches.clean %>% select(start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate)

cluster.data.2 <- na.omit(cluster.data.2)

cluster.data.2 <- scale(cluster.data.2)
>>>>>>> 70d48cd39d939288e32b909615f5caa2634eaf28

# save id numbers for merging later
idNumForSave <- cluster.data.2$idNum
cluster.data.2 <- select(cluster.data.2, -idNum)

<<<<<<< HEAD
cluster.data.2 <- as.data.frame(scale(cluster.data.2))

# Extract cluster id's for each pitch
library(cluster)
library(fpc)
pitches.pamk <- pamk(cluster.data.2)
# pitches.clean has all the pitching data, as well as the cluster id's

pitches.dist <- dist(cluster.data.2)
pitches.hclust <- hclust(pitches.dist)
pitches.hclust

try <- cutree(pitches.hclust, k = 7)
cluster.data.2$cluster_id <- try
cluster.data.2$idNum <- idNumForSave

pitches.clean3 <- merge(cluster.data.2, pitches.clean2, by = "idNum")
tb <- table(pitches.clean3$cluster_id, pitches.clean3$pitch_type)
# this table helps show which type of pitch is each cluster
tb2 <- prop.table(tb, margin = 1)


# using k-means clustering

set.seed(16)
index <- sample(1:nrow(pitchFX), nrow(pitchFX))
pitchFX$pitch_type <- as.factor(pitchFX$pitch_type)
mixed.pitches <- pitchFX[index,]


pitches.clean <- mixed.pitches
pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)

duplicates <- duplicated(t(pitches.clean))
pitches.clean2 <- pitches.clean[,!duplicates]
pitches.clean2$idNum <- index

pitches.vars <- pitches.clean2 %>% select(pitch_type, start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate)
pitches.vars2 <- na.omit(pitches.vars)
type <- pitches.vars2$pitch_type
pitches.vars2 <- select(pitches.vars2, -pitch_type)

percent25 <- 0.25 * nrow(pitches.vars2)
pitches.train <- pitches.vars2[(percent25 + 1) : nrow(pitches.vars2),]
pitches.test <- pitches.vars2[1:percent25, ]

pitches.train.target <- type[(percent25 + 1) : nrow(pitches.vars2)]
pitches.test.target <- type[1:percent25]
library(class)

m1 <- knn(train = pitches.train, test = pitches.test, cl = pitches.train.target, prob = T, k = 3)
confusionMatrix(m1, pitches.test.target)



# predict the outcomes
outcome <- pitches.clean2  %>%  select(event, des, idNum)

outcome$des <- as.factor(outcome$des)
levels(outcome$des) <- c("Ball", "Ball", "Called Strike", "Foul", "Foul", "Foul", "Foul", "Ball", "In play, no out", "In play, out", "In play, no out", "Swinging Strike", "Swinging Strike", "Swinging Strike")
outcome$des <- as.factor(outcome$des)

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

pitches.outcomes <- merge(pitches.clean2, outcome, by = "idNum")
pitches.outcomes <- pitches.outcomes %>% filter(end != "Bunt Groundout", end != "Bunt Pop Out", end != "Double Play", end != "Field Error", end != "Sac Bunt")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)
levels(pitches.outcomes$end) <- c("Ball", "Called Strike", "Double", "Groundout", "Flyout", "Groundout","Foul","Groundout","Groundout","Home Run","Lineout","Pop Out","Sac Fly","Single","Swinging Strike","Triple")
pitches.outcomes$end <- as.factor(pitches.outcomes$end)
=======
# Extract cluster id's for each pitch
library(cluster)
baseball.pam <- pam(cluster.data.2, 2)
pitches.clean$cluster_id <- baseball.pam$clustering

# Visualze clusters/pitch types
table(pitches.clean$pitch_type, pitches.clean$cluster_id)

# Organize the play outcomes into 5 play results
pitches.clean$des <- as.factor(pitches.clean$des)
levels(pitches.clean$des) <- c("Ball", "Ball", "Called Strike", "Foul", "Foul", "Foul", "Foul", "Ball", "In play, no out", "In play, out", "In play, no out", "Swinging Stirke")

# Seperate out by grid location
pitches.clean$grid <- sapply(pitches.clean, function(x) {
  pitches.clean$grid <- paste(pitches.clean$cluster_id, "_", pitches.clean$zone, sep = "")
})
>>>>>>> 70d48cd39d939288e32b909615f5caa2634eaf28

# Seperate pitches out by cluster
cluster.1 <- pitches.clean %>% filter(cluster_id == 1)
cluster.2 <- pitches.clean %>% filter(cluster_id == 2)

# See what proportion is gucci
prop.table(table(cluster.1$zone, cluster.1$des))
prop.table(table(cluster.2$zone, cluster.2$des))
