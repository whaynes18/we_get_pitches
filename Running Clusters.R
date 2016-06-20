library(pitchRx)
library(dplyr)
library(magrittr)
library(plyr)
library(cluster)

data("pitches")

ggplot(pitches, aes(x = x, y = y)) + geom_point(aes(color = type))
ggplot(pitches, aes(x = x, y = y)) + geom_point(aes(color = des))

# Clean Pitches Data
pitches.clean <- pitches
pitches.clean$break_y <- as.numeric(pitches.clean$break_y)
pitches.clean$break_angle <- as.numeric(pitches.clean$break_angle)
pitches.clean$break_length <- as.numeric(pitches.clean$break_length)
pitches.clean$stand <- as.factor(pitches.clean$stand)

# Cluster the Pitches Data based on characteristics (including location)
cluster.data.1 <- pitches.clean %>% select(x, y, start_speed, end_speed, sz_top, sz_bot, pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate)

set.seed(7)
ratios.1 = sapply(1:10, function(k) {
  with(kmeans(cluster.data.1, k, nstart = 5), betweenss / totss)
})
plot(ratios.1 ~ c(1:10))

library(cluster)
baseball.pam <- pam(cluster.data.1, 6)

# Cluster pitches data on characteristics (without location)
cluster.data.2 <- pitches.clean %>% select(start_speed, end_speed, pfx_x, pfx_z, vx0, vy0, vz0, break_y, break_angle, break_length, spin_dir, spin_rate)
cluster.data.2$break_y <- as.numeric(cluster.data.2$break_y)
cluster.data.2$break_angle <- as.numeric(cluster.data.2$break_angle)
cluster.data.2$break_length <- as.numeric(cluster.data.2$break_length)

set.seed(7)
ratios.2 = sapply(1:10, function(k) {
  with(kmeans(cluster.data.2, k, nstart = 10), betweenss / totss)
})
plot(ratios.2 ~ c(1:10))


# Extract cluster id's for each pitch
library(cluster)
baseball.pam <- pam(cluster.data.2, 3)
pitches.clean$cluster_id <- baseball.pam$clustering

# pitches.clean has all the pitching data, as well as the cluster id's






