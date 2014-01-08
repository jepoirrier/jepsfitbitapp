# Script to get data on sleep from Fitbit
# Requires a key + secret in jepsfitbitapp.pwd

library(ggplot2)
library(httr)
library(jsonlite)

# Get access parameters
pwds = read.table("jepsfitbitapp.pwd", sep="\t")
consumerKey = pwds[1,2]
consumerSecret = pwds[2,2]
URLRequest = pwds[3,2]
URLAccess = pwds[4,2]
URLAuthorize = pwds[5,2]

# Get access
fbr = oauth_app('JepsFitbitApp', consumerKey, consumerSecret)
fitbit = oauth_endpoint(URLRequest, URLAuthorize, URLAccess)
token = oauth1.0_token(fitbit, fbr)
sig = sign_oauth1.0(fbr, token=token$oauth_token, token_secret=token$oauth_token_secret)

# ##### SLEEP

# Some initializations
startDate = as.Date("2013-03-06") # I started using Fitbit on March 6th, 2013
endDate = as.Date("2013-12-31")
today = startDate
i = 1

dc = as.integer(endDate-startDate) # daysCounted
sleepDF = data.frame(awakeningsCount = numeric(dc),
                     efficiency = numeric(dc),
                     minutesAfterWakeup = numeric(dc),
                     minutesAsleep = numeric(dc),
                     minutesAwake = numeric(dc),
                     minutesToFallAsleep = numeric(dc),
                     #startTime = character(dc),
                     timeInBed = numeric(dc))

# Loop to collect all sleep parameters at all days
while(today <= endDate) {
  print(as.character(today))
  getString = paste("http://api.fitbit.com/1/user/-/sleep/date/", as.character(today), ".json", sep="")
  
  # Get the data
  Xsleep = GET(getString, sig)
  
  if(as.integer(Xsleep[3]$status_code[1]) != 200) {
    stop(paste("Return code for date", as.character(today), "not OK, please investigate."))
  }
  
  # Now rebuilding the json string
  # steps[6] contains each char in hexa --> translate into normal string
  myJson = rawToChar(Xsleep[6]$content[1])
  jsonLength = length(Xsleep[6]$content)
  for(j in 2:jsonLength) {
    myJson <- paste(myJson, rawToChar(Xsleep[6]$content[j]), sep="")
  }
  
  # Saving the json string (just to reopen it later - a better way?)
  cat(myJson, file="mySleep.dat")
  mySleep = fromJSON("mySleep.dat")
  
  # Extracting data from "sleep" and setting right data types
  if(mySleep$`summary`$totalSleepRecords != 0) {
    sleep = mySleep$`sleep`
    awakeningsCount = as.numeric(sleep$awakeningsCount)
    efficiency = as.numeric(sleep$efficiency)
    minutesAfterWakeup = as.numeric(sleep$minutesAfterWakeup)
    minutesAsleep = as.numeric(sleep$minutesAsleep)
    minutesAwake = as.numeric(sleep$minutesAwake)
    minutesToFallAsleep = as.numeric(sleep$minutesToFallAsleep)
    startTime = as.character(sleep$startTime)
    timeInBed = as.numeric(sleep$timeInBed)
    
    #sleepDF[i,] = c(awakeningsCount, efficiency, minutesAfterWakeup, minutesAsleep, minutesAwake, minutesToFallAsleep, startTime, timeInBed)
    sleepDF[i,] = c(awakeningsCount, efficiency, minutesAfterWakeup, minutesAsleep, minutesAwake, minutesToFallAsleep, timeInBed)
  }
  
  # Don't forget to increment the *two* counters!
  today = today + 1
  i = i + 1
  
  # "The current rate limiting quota is set at 150 calls per hour" at Fitbit
  # So you have to find a trick: start fresh and leave that for data for a year
  # 3,600 s per hour / 150 calls = 24 s btw calls --> 30 s sleep seems ok
  Sys.sleep(30)
}

# Now build the different graphs

sleepDF2 = sleepDF[sleepDF$minutesAsleep > 0, ]
summary(sleepDF2)

# Minutes asleep
x = sleepDF2$minutesAsleep/60
xLength = length(x)
plot(1:xLength, x, main="Hours asleep (March - December 2013)", xlab="Days in 2013", ylab="Hours asleep")

# It seems there is a trend to increase over time, try a fit?
lmfit = lm(x ~ c(1:xLength))
lmfit
summary(lmfit) # so apparently not significant
plot(lmfit)

# By plotting the histogram it seems this distribution look normal; check it!
hist(x, main="Histogram of hours asleep (March-December 2013)")
plot(density(x), main="Density estimates of hours asleep (03-12/2013)")
plot(ecdf(x), main="Empirical cumulative distribution of hours asleep (03-12/2013)")

z.norm = (x-mean(x))/sd(x) # standardized data
qqnorm(z.norm) # drawing QQplot
abline(0,1) # drawing 45 degree reference line

shapiro.test(x) # normality test

# plot both data and normal distribution
h = hist(x, breaks=30)
xhist = c(min(h$breaks), h$breaks)
yhist = c(0, h$density, 0)
xfit = seq(min(x), max(x), length=40)
yfit = dnorm(xfit, mean=mean(x), sd=sd(x))
plot(xhist, yhist, type="s", ylim=c(0, max(yhist, yfit)), main="Normal PDF and histogram of hours asleep (3-12/2013)", xlab="Hours asleep", ylab="Frequencies")
lines(xfit, yfit, col="red")

# Awakenings
x = sleepDF2$awakeningsCount
xLength = length(x)
xL = c(1:xLength)
qplot(xL, x, main="# awakenings (March - December 2013)", xlab="Days in 2013", ylab="# awakenings")
qplot(x, main="Distribution of awakenings (March - December 2013)", xlab="# of awakenings")

# minutesAsleep and awakenings are linked?
x = sleepDF2$minutesAsleep
y = sleepDF2$awakeningsCount
lmfit = lm(y ~ x)
lmfit
xLength = max(x)
xfit = c(1:xLength)
yfit = lmfit$coefficients[1] + lmfit$coefficients[2] * xfit
plot(x, y, main="Sleep duration & # of awakenings, 2013", xlab="Minutes asleep", ylab="# of awakenings")
lines(xfit, yfit, col="red")

# sleepEfficiency
x = sleepDF2$efficiency
xLength = length(x)
xL = c(1:xLength)
qplot(xL, x, main="Sleep efficiency (March - December 2013)", xlab="Days in 2013", ylab="Sleep efficiency (%)")

# TODO embed more stats, explore other parameters
