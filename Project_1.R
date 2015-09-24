# Set the directory
setwd("C:/Users/I039307/Documents/Hashin/01_Official/40_Data_Scientist/Reproducible Research/Project_1")
# Read the data
filepath<- "./activity.csv"
activitydata <- read.csv(filepath, header = TRUE)

# Calculate the sum by date
library(dplyr)
sumbydate <- activitydata %>% filter(steps != "NA") %>% group_by (date) %>% summarize(StepsDay = sum (steps))

# Histogram
hist(sumbydate$StepsDay, xlab = "date", ylab = "Total Steps")

# Mean steps
meanSteps <- mean(sumbydate$StepsDay)

# MedianSetps
medianSteps <- median(sumbydate$StepsDay)

# timeseries
timeseriesdata <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)

# Plot
plot(row.names(timeseriesdata), timeseriesdata, type = "l", xlab = "5-min interval", 
     ylab = "Average across all Days", main = "Average number of steps taken", 
     col = "red")

# Max steps
maxsteps <- which.max(timeseriesdata)
names(maxsteps)

# Calculate the mean by interval
meanbyinterval <- activitydata %>% filter(steps != "NA") %>% group_by (interval) %>% summarize(meanbyinterval = mean (steps))

# Replacing NA
tsteps <- numeric()
cactivitydata <- activitydata
for(i in 1:nrow(cactivitydata)){
    rowdata <- cactivitydata[i,]
    if (is.na(rowdata$steps)){
      steps <- subset(meanbyinterval, interval == rowdata$interval)$meanbyinterval
    }
    else
    {
      steps <- rowdata$steps
    }
    tsteps <- c(tsteps, steps)

}
cactivitydata$steps <- tsteps

# Calculate the sum by date
sumbydate2 <- cactivitydata %>% filter(steps != "NA") %>% group_by (date) %>% summarize(StepsDay = sum (steps))

# Histogram
hist(sumbydate2$StepsDay, xlab = "date", ylab = "Total Steps")

# Mean steps
meanSteps <- mean(sumbydate2$StepsDay)

# MedianSetps
medianSteps <- median(sumbydate2$StepsDay)

# Getting weekday or weekend information

daylevel <- vector()
for (i in 1:nrow(cactivitydata)) {
  activity <- cactivitydata[i,]
  day <- weekdays(as.Date(activity$date))
  if (day == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
cactivitydata$daylevel <- daylevel
cactivitydata$daylevel <- factor(cactivitydata$daylevel)

meanbydate2 <- cactivitydata %>% group_by (daylevel, interval) %>% summarize(Stepsmean = mean (steps))

library(lattice)
xyplot(Stepsmean ~ interval | daylevel, meanbydate2, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
