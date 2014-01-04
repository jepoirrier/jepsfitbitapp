# Script to generate list of dates (as string) between 2 given dates (days)
# To be used in sleep.R later on

startDate = as.Date("2013-03-06")
endDate = as.Date("2013-04-12")

today = startDate

while(today <= endDate) {
  print(as.character(today))
  today = today + 1
}

