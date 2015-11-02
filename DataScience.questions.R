# Q1. What is the probability that the tourist is at least 3 city blocks 
# (as the crow flies) from Broadway and Broadway after 10 moves?
library(partitions)
rm(list = ls())
moves = 10
blocks = 3
moves_dir <- data.frame(matrix(t(compositions(moves, 4)), ncol = 4))
colnames(moves_dir) <- c('left', 'right', 'up', 'down')
moves_dir$Prob <- apply(moves_dir, 1, dmultinom, prob = rep(.25, 4))
moves_dir$distance <- sqrt((moves_dir$left - moves_dir$right)^2 + 
    (moves_dir$up - moves_dir$down)^2)
prob <- sum(moves_dir$Prob[moves_dir$distance >= blocks])
sprintf('%.10f', prob)  # 0.4539794922

# Q2.What is the probability that the tourist is at least 10 city blocks 
# (as the crow flies) from Broadway and Broadway after 60 moves?
rm(list = ls())
moves = 60
blocks = 10
moves_dir <- data.frame(matrix(t(compositions(moves, 4)), ncol = 4))
colnames(moves_dir) <- c('left', 'right', 'up', 'down')
moves_dir$Prob <- apply(moves_dir, 1, dmultinom, prob = rep(.25, 4))
moves_dir$distance <- sqrt((moves_dir$left - moves_dir$right)^2 + 
    (moves_dir$up - moves_dir$down)^2)
prob <- sum(moves_dir$Prob[moves_dir$distance >= blocks])
sprintf('%.10f', prob)  # 0.2065011181

# Q3 & Q4
# Q3. What is the probability that the tourist is ever at least 5 city blocks 
# (as the crow flies) from Broadway and Broadway within 10 moves?
# Q4. What is the probability that the tourist is ever at least 10 city blocks 
# (as the crow flies) from Broadway and Broadway within 60 moves?
library(parallel)
rm(list = ls())
gc()
rw <- function(moves) {
  x_dir = c(-1, 1, 0, 0)  # left: -1. right: 1
  y_dir = c(0, 0, -1, 1)  # down: -1, up: 1
  moves_dir = sample(1:4, moves, replace = T)
  
  x_moves = x_dir[moves_dir]
  y_moves = y_dir[moves_dir]
  
  x_dist = cumsum(x_moves)
  y_dist = cumsum(y_moves)
  xy_dist = sqrt(x_dist^2 + y_dist^2)
  
  return(max(xy_dist))
}

moves = 10
blocks = 5

# parallel simulation 30 * 10^7 times
system.time(probs <- mclapply(rep(moves, 30), function(x) {
  dist <- replicate(10^7, rw(moves = x))
  prob <- sum(dist >= blocks) / length(dist)
}, mc.cores = 30))  # 600 second

sprintf('%.10f', mean(unlist(probs))) # Q3 0.1335052667

moves = 60
blocks = 10

system.time(probs <- mclapply(rep(moves, 30), function(x) {
  dist <- replicate(10^7, rw(moves = x))
  prob <- sum(dist >= blocks) / length(dist)
}, mc.cores = 30))  # 700 second

sprintf('%.10f', mean(unlist(probs))) # Q4 0.3184300500

# Q5 & Q6. 
# What is the probability that the tourist is ever east of East 1st Avenue 
# but ends up west of West 1st Avenue in 10 moves?
# What is the probability that the tourist is ever east of East 1st Avenue 
# but ends up west of West 1st Avenue in 30 moves?
rm(list = ls())
gc()
rw <- function(moves) {
  x_dir = c(-1, 1, 0, 0)  # left: -1. right: 1
  y_dir = c(0, 0, -1, 1)  # down: -1, up: 1
  moves_dir = sample(1:4, moves, replace = T)
  
  x_moves = x_dir[moves_dir]
  y_moves = y_dir[moves_dir]
  
  x_dist = cumsum(x_moves)
  
  rs_boolean = (max(x_dist > 1)) & (x_dist[length(x_dist)] < -1)
  return(rs_boolean)
}

moves = 10
system.time(probs <- mclapply(rep(moves, 30), function(x) {
  rs_boolean <- replicate(10^7, rw(moves = x))
  prob <- mean(rs_boolean)
}, mc.cores = 30))

sprintf('%.10f', mean(unlist(probs))) # Q5 0.0059103367

moves = 30
system.time(probs <- mclapply(rep(moves, 30), function(x) {
  rs_boolean <- replicate(10^7, rw(moves = x))
  prob <- mean(rs_boolean)
}, mc.cores = 30))

sprintf('%.10f', mean(unlist(probs))) # Q6 0.0774871433

# Q7 & Q8
# What is the average number of moves until the first time the tourist is 
# at least 10 city blocks (as the crow flies) from Broadway and Broadway.
# What is the average number of moves until the first time the tourist is 
# at least 60 city blocks (as the crow flies) from Broadway and Broadway.
library(parallel)
rm(list = ls())
gc()
rw <- function(moves, blocks) {
  x_dir = c(-1, 1, 0, 0)  # left: -1. right: 1
  y_dir = c(0, 0, -1, 1)  # down: -1, up: 1
  moves_dir = sample(1:4, moves, replace = T)
  
  x_moves = x_dir[moves_dir]
  y_moves = y_dir[moves_dir]
  
  x_dist = cumsum(x_moves)
  y_dist = cumsum(y_moves)
  xy_dist = sqrt(x_dist^2 + y_dist^2)
  n_moves = which(xy_dist >= blocks)[1]  
  return(n_moves)
}

# Q7
moves = 10000
blocks = 10

system.time(n_moves <- mclapply(rep(moves, 30), function(x) {
  n_move <- replicate(10^5, rw(moves = moves, blocks = blocks))
  n_move_avg = mean(n_move, na.rm = T)
}, mc.cores = 30))  # 330 second

sprintf('%.10f', mean(unlist(n_moves))) # Q7 104.6449173333

# Q8
moves = 10000
blocks = 60

system.time(n_moves <- mclapply(rep(moves, 30), function(x) {
  n_move <- replicate(10^5, rw(moves = moves, blocks = blocks))
  n_move_avg = mean(n_move, na.rm = T)
}, mc.cores = 30))  # 330 second

sprintf('%.10f', mean(unlist(n_moves))) # Q8 3359.7048657601
# it can be approciate with blocks^2 == 3600

## nyc 311 call
library(data.table)
library(dplyr)
data <- fread('/NAS/jhuang/Projects/DataIncubator/nyc311calls.csv')
df <- data.frame(data)

# Q1. What fraction of complaints are associated with the 2nd most popular agency?
complain_agency <- count(df, Agency, sort = TRUE)
ratio_2 <- complain_agency$n[2] / sum(complain_agency$n)
sprintf('%.10f', ratio_2)  # 0.1719314121

# Q2. What is the distance (in degrees) between the 90% and 10% percentiles of
# degrees latitude?
lat_percentile <- quantile(df$Latitude, c(0.1, 0.9), na.rm = TRUE)
lat_dif <- lat_percentile[2] - lat_percentile[1]
sprintf('%.10f',lat_dif)  # 0.2357908310


# Q3. What is the difference between the expected number of calls received during 
# the most and least popular whole hours of the day? (Remove points which do not 
# seem to accurately reflect the actual time they were reported.)
Time <- strptime(df$Created.Date, '%m/%d/%Y %I:%M:%S %p')
sum(is.na(Time))  # output is 0, indicating all of them can be converted to time

# remove the time that is '* 00:00:00', which acounts for several million records
# That seems inaccurate compared to other timestamp
Time_accurate <- Time[!grepl('00:00:00', Time)]  
Hours <- format(Time_accurate, '%H')
Hours_freq <- as.data.frame(table(Hours))
n_days <- as.Date(max(Time_accurate)) - as.Date(min(Time_accurate)) + 1
Freq_dif <- (max(Hours_freq$Freq) - min(Hours_freq$Freq)) / as.integer(n_days)
sprintf('%.10f',Freq_dif)  # 237.6725285171

# Q4. What is the most 'surprising' complaint type when conditioned on a borough? 
# That is, what is the largest ratio of the conditional probability of a complaint 
# type given a specified borough divided by the unconditioned probability of that
# complaint type?
# n.complaint: number of complaint for each complaint type
# n.bor: number of complaint in each borough
bor_complaint <- count(df, Complaint.Type, Borough)
bor_complaint$n.bor <- ave(bor_complaint$n, bor_complaint$Borough, FUN = sum)
bor_complaint$n.complaint <- ave(bor_complaint$n, bor_complaint$Complaint.Type, FUN = sum)
bor_complaint$uncond_p <- bor_complaint$n.complaint / sum(bor_complaint$n)
bor_complaint$cond_p <- bor_complaint$n / bor_complaint$n.bor
surprising_r <- max(bor_complaint$cond_p / bor_complaint$uncond_p)
sprintf('%.10f',surprising_r)  # 18.2636539395

# Q5. Let's estimate the area that 311 supports. Suppose calls are 2D normally 
# distributed on the surface of the earth with mean and standard deviation given 
# by those of the latitude and longitude. How many square kilometers is the 
# single-standard-deviation ellipse?
lat_sd = sd(df$Latitude, na.rm = TRUE)
lon_sd = sd(df$Longitude, na.rm = TRUE)
lat_mean = mean(df$Latitude, na.rm = TRUE)
lon_mean = mean(df$Longitude, na.rm = TRUE)

fun.gcd <- function(lon1, lat1, lon2, lat2) { 
  # calculate the great circle distance (km)
  lon1 = lon1 * pi / 180
  lat1 = lat1 * pi / 180
  lon2 = lon2 * pi / 180
  lat2 = lat2 * pi / 180
  R <- 6371 # km, Earth mean radius 
  d <- acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon2-lon1)) * R
  return(d)
}

ellipse_a = fun.gcd(lon_mean - lon_sd, lat_mean, lon_mean + lon_sd, lat_mean) / 2
ellipse_b = fun.gcd(lon_mean, lat_mean - lat_sd, lon_mean, lat_mean + lat_sd) / 2

area <- pi * ellipse_a * ellipse_b
sprintf('%.10f', area)  # 210.0373982522


# Q6. What is the standard deviation in seconds of the time between consecutive 
# calls? (Remove points which do not seem to accurately reflect the actual time
# they were reported.)
Time_sort <- sort(Time_accurate)
interval <- diff(Time_sort) 
second_sd <- sd(interval)  
sprintf('%.10f', second_sd)  # 64.3430326760

# no complaint between  02:21:36 AM to 11:56:00 PM on 10/05/2015?
# The # The max interval occuring on 10/05/2015 11:56:00 PM seems an outlier
# if the max interval is removed, the sd will change a lot 
second_sd_rm_largest <- sd(interval[-length(interval)])
sprintf('%.10f', second_sd_rm_largest)  # 56.6804780473