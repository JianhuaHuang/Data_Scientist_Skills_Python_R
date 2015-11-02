# rm(list =ls())
.libPaths('/NAS/jhuang/R/x86_64-redhat-linux-gnu-library/3.2')
moves = 10
blocks = 3
moves_dir <- diff(rbind(0, combn(moves, 3), moves))
moves_dir <- data.frame(t(moves_dir))
colnames(moves_dir) <- c('left', 'right', 'up', 'down')
moves_dir$distance <- sqrt((moves_dir$left - moves_dir$right)^2 + (moves_dir$up - moves_dir$down)^2)
prob <- sum(moves_dir$distance >= blocks) / nrow(moves_dir)
sprintf('%.10f', prob)  # 0.6583333333

moves = 60
blocks = 10
moves_dir <- diff(rbind(0, combn(moves, 3), moves))
moves_dir <- data.frame(t(moves_dir))
colnames(moves_dir) <- c('left', 'right', 'up', 'down')
moves_dir$distance <- sqrt((moves_dir$left - moves_dir$right)^2 + (moves_dir$up - moves_dir$down)^2)
prob <- sum(moves_dir$distance >= blocks) / nrow(moves_dir)
sprintf('%.10f', prob)  # 0.8896843951

## nyc 311 call
library(data.table)
library(dplyr)
data <- fread('/NAS/jhuang/Projects/DataIncubator/nyc311calls.csv')
df <- data.frame(data)

# Q1. What fraction of complaints are associated with the 2nd most popular agency?
complain_agency <- count(df, Agency, sort = TRUE)
ratio_2 <- complain_agency$n[2] / sum(complain_agency$n)
sprintf('%.10f', ratio_2)  # 0.1719314121

# Q2. What is the distance (in degrees) between the 90% and 10% percentiles of degrees latitude?
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

# project description likn
# https://youtu.be/masXhcwhY2c