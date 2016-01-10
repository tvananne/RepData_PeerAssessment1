

#this is my script for Project 1 of Reproducible Research:

#set enviornment
  #setwd("") #set your wd here
  require("dplyr")
  require("ggplot2")
  #might need to install these if you haven't yet
    #install.packages("dplyr")
    #install.packages("ggplot2")
  library(ggplot2)
  library(dplyr)
  
# 1) Loading and Preprocessing Data
  activity <- read.csv("data/activity.csv", stringsAsFactors=FALSE)
  #proc if necessary (not yet)
  activity$dateFactor <- as.factor(activity$date)
  act <- activity
  
# 2) Calculate: Total numb steps per day / Mean and Median steps per day

  #sum of steps per day
  act.grouped <- group_by(act, dateFactor)
  act.sum <- summarize(act.grouped, 'sum_steps'=sum(steps, na.rm = TRUE))  
  
  #histogram
  hist(act.sum$sum_steps, main = "Histogram of Steps Per Day", xlab = "sum of steps per day")
  
  #average of steps per day
  average.steps.per.day <- mean(act.sum$sum_steps, na.rm = TRUE)
  average.steps.per.day
  
  #median of steps per day
  median.steps.per.day <- median(na.omit(act.sum$sum_steps))
  median.steps.per.day
  
  rm(act.grouped)
  
# 3) What is the average daily activity pattern?
  
  act.dailyacty <- group_by(act, interval)
  act.dailyacty <- summarize(act.dailyacty, 'average_per_interval'=mean(steps, na.rm = TRUE))
  act.dailyacty
  
  plot(act.dailyacty$interval, act.dailyacty$average_per_interval, type="l", xlab = "5 Second Intervals (24 hours)",
       ylab = "Average Number of Steps", main = "Average Number of Steps per 5-Second Interval")
  
  max_steps_per_interval <- act.dailyacty[which(act.dailyacty$average_per_interval == max(act.dailyacty$average_per_interval)), ]
  max_steps_per_interval
  
  
# 4) Impute missing values (from the mean for that 5 min interval)
  #first lets get a count of how many values we're dealing with that are missing!
  act.rmnas <- act
  sum(is.na(act.rmnas$steps))
  
  #ok, going to use the means from act.dailyacty and map those to missing values in act.rmnas
  len1 <- nrow(act.rmnas)
  
  #use act.dailyacty as a lookup value for imputing NA values
  for(i in 1:len1) {
    if (is.na(act.rmnas[i, 1])) {
      print(i)
      print("in first loop")
      print(act.rmnas[i, 1]) 
      
      #lookup in act.dailyacty
      tempLookup <- which(act.dailyacty$interval == act.rmnas[i, 3])
      act.rmnas[i, 1] = act.dailyacty[tempLookup, 2]
    }
  }
  
  #finally, lets recount how many NAs there are (should be zero)
  sum(is.na(act.rmnas$steps))


# 5) Calculate: Total numb steps per day / Mean and Median steps per day
  
  #sum of steps per day
  act.rmnas.grouped <- group_by(act.rmnas, dateFactor)
  act.rmnas.sum <- summarize(act.rmnas.grouped, 'sum_steps'=sum(steps))  #, 'average steps'=mean(steps), 'median steps'=median(steps))
  
  #histogram
  hist(act.rmnas.sum$sum_steps, main = "Histogram of Steps Per Day (NAs Imputed)", xlab = "sum of steps per day")
  
  #average of steps per day (imputed NAs)
  average.steps.per.day.rmnas <- mean(act.rmnas.sum$sum_steps)
  average.steps.per.day.rmnas
  
  #median of steps per day (imputed NAs)
  median.steps.per.day.rmnas <- median(na.omit(act.rmnas.sum$sum_steps))
  median.steps.per.day.rmnas
  
  #What is the average daily activity pattern?
  
  act.rmnas.dailyacty <- group_by(act.rmnas, interval)
  act.rmnas.dailyacty
  act.rmnas.dailyacty <- summarize(act.rmnas.dailyacty, 'average_per_interval'=mean(steps, na.rm = TRUE))
  act.rmnas.dailyacty
  
  plot(act.rmnas.dailyacty$interval, act.rmnas.dailyacty$average_per_interval, type="l", xlab = "5 Second Intervals (24 hours)",
       ylab = "Average Number of Steps", main = "Average Number of Steps per 5-Second Interval (NAs Imputed)")
  
  max_steps_per_interval.rmnas <- act.rmnas.dailyacty[which(act.rmnas.dailyacty$average_per_interval == max(act.rmnas.dailyacty$average_per_interval)), ]
  max_steps_per_interval.rmnas  #interval with maximum steps
  

# 6) Are there any differences between weekdays and weekends?
  
  #lets use act.rmnas as our fact table
  act.rmnas$date <- as.Date(act.rmnas$date)
  act.rmnas$day <- "WEEKDAY OR WEEKEND WILL GO HERE"
  
  str(act.rmnas)
  
  #determine if Weekend or Weekday record
  for(k in 1:nrow(act.rmnas)){ 
    if(weekdays(act.rmnas[k, 2]) == "Saturday" | weekdays(act.rmnas[k, 2]) == "Sunday") {
      act.rmnas[k, 5] <- "Weekend"
    } else {
      act.rmnas[k, 5] <- "Weekday"
    }
  }
  
  unique(act.rmnas$day)
  
  #group by both interval and whether it is weekday or weekend
  act.rmnas.daygroups <- group_by(act.rmnas, interval, day)
  act.rmnas.daygroups <- summarize(act.rmnas.daygroups, 'average_steps'=mean(steps))
  
  
  myplot <- ggplot(act.rmnas.daygroups, aes(x=interval, y=average_steps)) + geom_line()
  myplot + facet_grid(day ~ .)
  
  
  