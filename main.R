# -------------------------- LOAD LIBRARIES -----------------------------------

library(dplyr)
library(geosphere)
library(ggplot2)
library(lubridate)
library(readr)
library(reticulate)
library(tidyverse)

# -------------------------- READ DATA ----------------------------------------

# set this variable to reflect the path to the .csv files
setwd("~/universiteit/hackathon/hackathoncode/telematics_data/")
data_files <- list.files(pattern = "*.csv")

# a data frame to store the results for each vehicle ID
results <- data.frame(NULL)

# iterate over each file and add its results
for (csv_file in data_files) {
  raw.data <- read_csv(csv_file)
  raw.data <- read.csv("75655786.csv")

# -------------------------- CLEAN DATA ---------------------------------------

# remove metadata
meta <- c("id", "vext", "terminal_id", "recieved_ts", "SP_CODE", "created_ts", "country_name")
data <- raw.data[, !names(raw.data) %in% meta]

# regularise time intervals - one observation every three minutes
# three minutes is the longest interval between observations
reg.data <- data %>% 
  mutate(timestamp = ymd_hms(timestamp), interval = floor(minute(timestamp) / 3)) %>% 
  group_by(date(timestamp), hour(timestamp), interval) %>%
  slice_head() %>%
  ungroup() %>% 
  select(-c("date(timestamp)", "hour(timestamp)", interval))

# -------------------------- INTERNAL -----------------------------------------

# -------------------------- RISKY --------------------------------------------
# Determine internal (i.e. related to the driver) indicators of risky driving
#  > high jerk (variability in acceleration)
#  > flag high acceleration
#  > speeding
#  > sharp turns

# ACCELERATION ----------------------------------------------------------------
# handle missing values
data$x_accel[is.na(data$x_accel)] <- mean(data$x_accel, na.rm = TRUE)
data$y_accel[is.na(data$y_accel)] <- mean(data$y_accel, na.rm = TRUE)
data$z_accel[is.na(data$z_accel)] <- mean(data$z_accel, na.rm = TRUE)

# combine acceleration components
data <- data %>%
  mutate(accel = sqrt(x_accel^2 + y_accel^2 + z_accel^2))

# plot acceleration on regular time intervals
reg.data <- reg.data %>%
  mutate(accel = sqrt(x_accel^2 + y_accel^2 + z_accel^2))

ggplot(data, aes(x = timestamp, y = accel)) + 
  geom_line() +
  labs(title ="Acceleration", x = "Time (s)", y = "Acceleration (m/s^2)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# JERK ------------------------------------------------------------------------
# derivative of acceleration w.r.t time
# jerk above a certain threshold for the speed driven indicates risky behavior

# calculate jerk
data <- data %>%
  mutate(accel.diff = as.numeric(accel - lag(accel, default = first(accel))))
data <- data %>%
  mutate(time.diff = as.numeric(timestamp - lag(timestamp, default = first(timestamp))))
data$jerk <- data$accel.diff / data$time.diff

# handle missing values (timediff = 0)
data$jerk[is.na(data$jerk)] <- 0

# plot jerk 
ggplot(reg.data, aes(x = timestamp, y = jerk)) + 
  geom_line() +
  labs(title = "Jerk", subtitle = "Derivative of acceleration w.r.t time", x = "Time (s)", y = "Jerk (mg/s)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# SPEEDING --------------------------------------------------------------------
# handle missing values: missing road_speed values indicate 
# that the vehicle is idling or no moving, i.e. not speeding
# interpolating missing values with a very large value ensures
# that the "speeding" variable is set to false
reg.data$road_speed[is.na(reg.data$road_speed)] <- 1000
reg.data <- reg.data %>% 
  mutate(speeding = (reg.data$speed > reg.data$road_speed))

# SHARP TURNS -----------------------------------------------------------------
reg.data$heading[is.na(reg.data$heading)] <- 0
reg.data <- reg.data %>% 
  mutate(turning = as.numeric(reg.data$heading - lag(reg.data$heading, default = first(reg.data$heading)))) %>% 
  mutate(turning = (turning > 90))

# ADD RESULTS -----------------------------------------------------------------
summed <- c(reg.data$vehicleid[1], sum(reg.data$speeding), mean(reg.data$accel), mean(reg.data$jerk), 
            sum(turning))
results <- rbind(results, summed)

# -------------------------- ERRATIC ------------------------------------------
# Determine a deviation from the normal driving pattern
# of a driver associated with a vehicle ID.
# This can indicate an inebriated, tired or ill driver, or that another,
# unapproved, driver is operating the vehicle


# -------------------------- EXTERNAL -----------------------------------------
# Determine external (i.e. related to the environment) indicators of
# irregularity
#  > sudden deviations from normal distance traveled for similar days of the 
#  week
#  > sudden deviations in speed traveled in a certain suburb

# DISTANCE PER DAY ------------------------------------------------------------
data$timestamp = as.Date(data$timestamp)
data$partition_date = as.Date(data$partition_date)

dist.daily <- NULL
dist.daily <- data %>% 
              group_by(partition_date) %>% 
              transmute(dist = (sum(distVincentyEllipsoid(cbind(data$coordinate_longitude, data$coordinate_latitude)))))
View(dist.daily)

# identify outliers here

# MEAN SPEED FOR SUBURB -------------------------------------------------------
sub.speed <- aggregate(data$speed, list(data$SP_NAME), FUN = mean)

# identify outliers here
        