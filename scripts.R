setwd("Z:\\odrive\\OneDrive-WatiSahnan\\Repos\\datasciencecoursera\\RepData_PeerAssessment1")

if ( !(file.exists("activity.csv"))) { 
    unzip("activity.zip")
}

library(dplyr)
library(ggplot2)
library(tibble)
library(hms)

# Loading and preprocessing the data
####################################
            activity <- read.csv("activity.csv")
       activity$date <- as.Date(activity$date)
activity$interval.fac <- as.factor(activity$interval)

dates <- as.character(activity$date)
times <- sprintf("%04d", activity$interval)
activity$date.time <- strptime(paste(dates, times), "%Y-%m-%d %H%M")

# exploratory functions
dim(activity)
head(activity)
summary(activity)
str(activity)

# exploratory questions
# - which interval has max/min number of steps?
# - what is the max/min number of stpes
#     - at which interval?
#     - on which day?
# - which day (date) has most/least number of total steps?
# - some intervals have 1 step:
#     - what does this mean? take 1 step and stop?
#     - does this 1 step occur at same interval each day?
# - weekdays vs weekend step counts

# max number of steps
max(activity$steps,na.rm = TRUE)
#[1] 806
maxStep <- subset(activity, steps==max(activity$steps,na.rm = TRUE))
# max number of steps occur on
maxStep
#       steps       date interval
# 16492   806 2012-11-27      615

# min number of steps
min(activity$steps[activity$steps>0], na.rm = TRUE)
# [1] 1
minStep <- subset(activity, steps==1)
# min number of steps occur on
minStep
#       steps       date interval
# 936       1 2012-10-04      555
# 1043      1 2012-10-04     1450
# 3974      1 2012-10-14     1905
# 5422      1 2012-10-19     1945
# 5837      1 2012-10-21      620
# 10755     1 2012-11-07      810
# 15072     1 2012-11-22      755

# exploratory functions with NAs
dim(totalStepsPerDay)
# [1] 61
names(totalStepsPerDay)

max(totalStepsPerDay)
# [1] 21194
highestDay <- totalStepsPerDay[totalStepsPerDay==max(totalStepsPerDay)]
highestDay
# 2012-11-23 
#      21194 

min(totalStepsPerDay[totalStepsPerDay > 0])
lowestDay <- totalStepsPerDay[totalStepsPerDay==min(totalStepsPerDay[totalStepsPerDay > 0])]
lowestDay
# 2012-11-15 
#         41

#hist(totalStepsPerDay, ylim = c(0,30),col="gray", breaks = 10) #ToDo:add titles

# What is mean total number of steps taken per day?
####################################################
totalStepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
#qplot(totalStepsPerDay, binwidth = 2000,  xlab = "total number of steps taken each day")
totalStepsPerDay.df <- as.data.frame(totalStepsPerDay)
ggplot(data=totalStepsPerDay.df,aes(x=totalStepsPerDay), na.rm=TRUE) + 
    geom_histogram(col="gray", fill="darkcyan", binwidth = 2000) + 
    labs(title = "Total Steps per Day") + 
    labs(x="Number of steps", y="Frequency") 
# + xlim(c(0,24000)) + ylim(c(0,13))
#, breaks=seq(0,22000, by=2000)
mean(totalStepsPerDay)
# [1] 9354.23
median(totalStepsPerDay)
# [1] 10395


# What is the average daily activity pattern?
##############################################

meanStepsPerInterval <- aggregate(x = list(steps = activity$steps), 
                      by = list(interval = activity$interval), 
                      mean, na.rm = TRUE)

ggplot(meanStepsPerInterval, aes(x = interval, y = steps)) + 
    geom_line(size=0.71,colour = "darkcyan") + 
    labs(x = "interval", 
         y = "Number of steps", 
         title = "Average Daily Activity Pattern") 

max(meanStepsPerInterval$steps)
activity$interval[which.max(meanStepsPerInterval$steps)]
#[1] 835

# Inputing missing values
#########################
missingData <- is.na(activity$steps)
sum(missingData)
table(missingData)
# FALSE  TRUE 
# 15264  2304

unique(activity$date[missingData])
# [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09" "2012-11-10"
# [7] "2012-11-14" "2012-11-30"


activity$interval_revised <- times
byInterval <- split(activity, activity$interval_revised) 
activity$average_steps <- sapply(byInterval, function(x) mean(x$steps, na.rm = TRUE))
activity$steps_revised <-  ifelse(is.na(activity$steps)
                                      , ceiling(activity$average_steps)
                                      , activity$steps)

missingRevisedData <- is.na(activity$steps_revised)
nrow(missingRevisedData)
# NULL

activityFilled <- data.frame(steps=activity$steps_revised,
                             date = activity$date,
                             interval = activity$interval)

table(is.na(activityFilled$steps))
totalFilledSteps <- tapply(activityFilled$steps, activityFilled$date, sum, na.rm=TRUE)
totalFilledSteps.df <- as.data.frame(totalFilledSteps)
ggplot(data=totalFilledSteps.df,aes(x=totalFilledSteps), na.rm=TRUE) + 
    geom_histogram(col="darkcyan", binwidth = 2000) + 
    labs(title = "Total Steps per Day with Inputted Data") + 
    labs(x="Number of steps", y="Frequency") 

#+ xlim(c(0,22000)) +  ylim(c(0,20))

# exploratory functions with inputted steps
dim(totalFilledSteps)
# [1] 61
names(totalFilledSteps)

max(totalFilledSteps)
# [1] 21194
highestDay <- totalFilledSteps[totalFilledSteps==max(totalFilledSteps)]
highestDay
# 2012-11-23 
#      21194 

min(totalFilledSteps[totalFilledSteps > 0])
lowestDay <- totalFilledSteps[totalFilledSteps==min(totalFilledSteps[totalFilledSteps > 0])]
lowestDay
# 2012-11-15 
#         41

mean(totalFilledSteps)
#[1] 10784.92
median(totalFilledSteps)
#[1] 10909

# Are there differences in activity patterns between weekdays and weekends?
###########################################################################

unique(weekdays(activityFilled$date))
#[1] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday"  "Sunday"

isWeekend <- c("Saturday", "Sunday")

weekdays(as.POSIXlt("2012-11-03")) #[1] "Saturday" CHAR
weekdays(as.POSIXlt("2012-11-04")) #[1] "Sunday"
weekdays(as.POSIXlt("2012-11-05")) #[1] "Monday"
strptime(as.POSIXlt("2012-11-03"), "%w") #[1] "2020-03-16 +08" FAILED
strptime("2012-11-04", "%w")             #[1] "2020-03-16 +08" FAILED
format(as.POSIXlt("2012-11-05"), "%w")   #[1] "1" CHAR
as.POSIXlt("2012-11-03")$wday # [1] 0  INT
as.POSIXlt("2012-11-04")$wday # [1] 6
as.POSIXlt("2012-11-05")$wday # [1] 1

weekdays(activityFilled$date)
isWeekend <- c(0, 6)
activityFilled$dayType <-  ifelse(as.POSIXlt(activityFilled$date)$wday %in% isWeekend, 'weekend', 'weekday')

activityFilledMean <- aggregate(steps ~ interval + dayType, data=activityFilled, mean)
ggplot(activityFilledMean, aes(interval, steps, color=factor(dayType))) + 
    geom_line(size=0.72) + 
    facet_grid(dayType ~ .) +
    xlab("interval") + 
    ylab("avarage number of steps") +
    theme(strip.background = element_rect(fill="oldlace"), legend.position = "none") +
    guides(fill="none")






