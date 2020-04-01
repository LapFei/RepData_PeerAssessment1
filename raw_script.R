##############Loading and preprocessing the data
act <- read.csv(unz('activity.zip', 'activity.csv'), header = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)

act <- act %>% tbl_df
act$date <- as.character(act$date) %>% as.Date('%Y-%m-%d')

###############What is mean total number of steps taken per day?
act_gpdate <- group_by(act, date)
stsum_d <- summarise(act_gpdate, sum = sum(steps), 
                     mean = mean(steps), median = median(steps, na.rm = T))

a <- ggplot(stsum_d, aes(date, sum))
a+geom_col()+labs(y = 'total steps in each day')
################What is the average daily activity pattern?
########time series plot for the average steps across days
act_gpint <- group_by(act, interval)
stsum_i <- summarise(act_gpint, mean = mean(steps, na.rm = TRUE))
qplot(interval, mean, data = stsum_i, geom = 'area')

#########the interval having the maximal mean value
filter(stsum_i, mean == max(mean))

###################################Imputing missing values
#####number of NA across rows
apply(act, 2, is.na) %>% apply(2, sum)
#####imputing the NA with the mean for the 5-min interval
imputed <- act
for(i in 1:nrow(imputed)){
      if(is.na(imputed$steps[i])){
            imputed$steps[i] <- rep(stsum_i$mean, time = nrow(imputed)/nrow(stsum_i))[i]
      }
}
im_gpd <- group_by(imputed, date)
qplot(date, steps, data = imputed, geom = 'col', xlab = 'total steps in each day')
##############mean and median of the imputed data
sumim <- summarise(im_gpd, mean = mean(steps), median = median(steps))

#########################Are there differences in activity patterns between weekdays and weekends?
weekd_e <- mutate(imputed, weekdays = (weekdays(act$date) == 'Saturatday'|weekdays(act$date) == 'Sunday' )%>%
      factor(labels = c('weekdays', 'weekend')))
weekde_gp <- group_by(weekd_e, weekdays, interval)
sumwkde <- summarise(weekde_gp, mean = mean(steps))
sumwkde
names(sumwkde)
b <- ggplot(sumwkde, aes(interval, mean))
b+geom_line()+labs(y = 'average steps taken')+facet_grid(weekdays~.)
act
