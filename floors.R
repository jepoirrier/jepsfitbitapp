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

# ##### FLOORS

# Get the data (here: floors - I started using Fitbit on March 6th, 2013)
Xfloors = GET("http://api.fitbit.com/1/user/-/activities/floors/date/2013-03-06/2013-12-25.json",sig)

# Now rebuilding the json string
# steps[6] contains each char in hexa --> translate into normal string
myJson = rawToChar(Xfloors[6]$content[1])
jsonLength = length(Xfloors[6]$content)
for(i in 2:jsonLength) {
  myJson <- paste(myJson, rawToChar(Xfloors[6]$content[i]), sep="")
}

# Saving the json string (just to reopen it later - a better way?)
cat(myJson, file="myFloors.dat")
myFloors = fromJSON("myFloors.dat")

# Extracting data frame "activities-steps" and setting right data types
floors = myFloors$`activities-floors`
floors[,"dateTime"] = as.Date(floors[,"dateTime"])
floors[,"value"] = as.integer(floors[,"value"])

# Plotting # floors
qplot(floors$dateTime, floors$value, main="Floors w/ Fitbits so far (2013)", xlab="Time", ylab="# floors")

# Some stats
floorsA = floors$value[floors$dateTime < as.Date("2013-07-01")]
floorsH = floors$value[floors$dateTime > as.Date("2013-07-01")]
floorsH = floorsH[1:194] # trick to get only Flex One data - better way?
t.test(floorsA, floorsH)

# TODO embed more stats
