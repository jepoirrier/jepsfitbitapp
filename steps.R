# Script to get data on steps from Fitbit
# Requires a key + secret in jepsfitbitapp.pwd

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

# Get the data (here: steps - I started using Fitbit on March 6th, 2013)
steps = GET("http://api.fitbit.com/1/user/-/activities/steps/date/2013-03-06/2013-03-07.json",sig)

dput(steps, "mySteps.dat")
mySteps = fromJSON("mySteps.dat")
