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

# Cluster the Pitches Data based on characteristics (including location)
duplicates <- duplicated(t(pitches.clean))
pitches.clean2 <- pitches.clean[,!duplicates]
pitches.clean2$idNum <- sample(1:nrow(pitches.clean2), nrow(pitches.clean2))


# Cluster pitches data on characteristics (without location)
cluster.data.2 <- pitches.clean2 %>% select(idNum, start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate)

cluster.data.2 <- na.omit(cluster.data.2)

# save id numbers for merging later
idNumForSave <- cluster.data.2$idNum
cluster.data.2 <- select(cluster.data.2, -idNum)

cluster.data.2 <- as.data.frame(scale(cluster.data.2))

# Extract cluster id's for each pitch
library(cluster)

# pitches.clean has all the pitching data, as well as the cluster id's

pitches.dist <- dist(cluster.data.2)
pitches.hclust <- hclust(pitches.dist)
pitches.hclust

try <- cutree(pitches.hclust, k = 12)
cluster.data.2$cluster_id <- try
cluster.data.2$idNum <- idNumForSave

pitches.clean3 <- merge(cluster.data.2, pitches.clean2, by = "idNum")
pitchOne <- filter(pitches.clean3, cluster_id == 1)
pitchTwo <- filter(pitches.clean3, cluster_id == 2)
pitchThree <- filter(pitches.clean3, cluster_id == 3)

