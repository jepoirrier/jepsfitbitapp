# Script to get data on steps from Fitbit
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

# ##### STEPS

# Get the data (here: steps - I started using Fitbit on March 6th, 2013)
Xsteps = GET("http://api.fitbit.com/1/user/-/activities/steps/date/2013-03-06/2013-12-25.json",sig)

# Now rebuilding the json string
# steps[6] contains each char in hexa --> translate into normal string
myJson = rawToChar(Xsteps[6]$content[1])
jsonLength = length(Xsteps[6]$content)
for(i in 2:jsonLength) {
  myJson <- paste(myJson, rawToChar(Xsteps[6]$content[i]), sep="")
}

# Saving the json string (just to reopen it later - a better way?)
cat(myJson, file="mySteps.dat")
mySteps = fromJSON("mySteps.dat")

# Extracting data frame "activities-steps" and setting right data types
steps = mySteps$`activities-steps`
steps[,"dateTime"] = as.Date(steps[,"dateTime"])
steps[,"value"] = as.integer(steps[,"value"])

# Plotting # steps
qplot(steps$dateTime, steps$value, main="Steps w/ Fitbits so far (2013)", xlab="Time", ylab="# steps")

# TODO embed stats
