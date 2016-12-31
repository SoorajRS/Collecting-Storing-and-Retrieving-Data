a <- read.csv("Acquisitions.csv",sep = ",")				#Read the csv file
a                                                       #CSV file
leastInvInterval <- function(a) {						#Function
  ad <- as.Date(a$Date,format="%m/%d/%Y")    			#Formatting date
  addiff <- diff(ad)                                    #Calculating differences in dates
  addiff <- as.numeric(addiff)   						#Storing the date difference as a number
  x <- addiff                                           #Calculating minimum value of interval
  return(min(x)) 
}
leastInvInterval(a)
