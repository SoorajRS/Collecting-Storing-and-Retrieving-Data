library(dplyr)  
library(lubridate)

# Question 1
bs <- read.csv("Bird Strikes.csv",sep = ",")    					#Reading the csv file
head(bs) 															#Verify whether data is loaded properly
class(bs)										 					#Verify whether it is a data frame
f1 <- function(bs){                                                 #Function to year when no bird strikes reported
      no <- bs$Reported..Date[which(bs$Reported..Date=="")] 		#Checking in data where date is not mentioned
      Noyear <- length(no)                                          #Finding how many dates are empty
      return(Noyear)
}
f1(bs)

# Question 2														#Function to calculate the year with most air strikes
f2 <- function(bs) {
      yr <- year(as.Date(bs$FlightDate, format = "%m/%d/%Y")) 		#Formatting Date
      yrsum<-aggregate(yr,by=list(yr),length) 						#In obtained data, group by year and find sum of occurrences
      colnames(yrsum)<- c("Year”,”Sum”)
      return(yrsum$Year[which(yrsum$Sum==max(yrsum$Sum))]) 			#Return year which has maximum bird strikes
}
f2(bs)

# Question 3														#Function to calculate bird strikes each year
f3 <- function(bs) {
      yr <- year(as.Date(bs$FlightDate, format = "%m/%d/%Y"))		#Formatting Date
      yrsum<-aggregate(yr,by=list(yr),length)						#In obtained data, group by year and find sum of occurrences
      colnames(yrsum)<- c("Year”,”Sum”)
      return(yrsum)
}
#Return entire group by year and values of bird strikes for the corresponding year
f3(bs)

# Question 4
f4 <- function(bs) {												#Function to calculate bird strikes per airline
      AirlineStrikesF <-aggregate(bs$Aircraft..Airline.Operator,by=list(bs$Aircraft..Airline.Operator),length) 	#Group by airline operator
      AirlineStrikesF
      class(AirlineStrikesF)
      sortedByAirlines <- arrange(AirlineStrikesF,desc(x))  		#Sorting the airlines in descending order
      return(sortedByAirlines)
}
AirlineStrikes <- f4(bs)											#AirlineStrikes
AirlineStrikes
#The output data frame from previous function as an argument in this function to return the airline with most bird strikes
f5 <- function(AirlineStrikes) {
      return(AirlineStrikes[2,1]) 									#Returning 2nd highest birdstrike as 1st is UNKNOWN
}
f5(AirlineStrikes)

#Data Scaling
data2x <- rbind(bs,bs)
data4x <- rbind(data2x,data2x)
data6x <- rbind(data4x,data2x)

# Question 5
#The Time complexity of the functions applied for question 1 to 3 is O(n).
#As the input size increases linearly the time require also increases linearly.
#For the Question 4 uses Sort so the complexity is O(n^2) assuming the sorting mechanism used is selection sort.

# Question 6
#Q1 - O(n)
system.time(f1(bs))
system.time(f1(data2x))
system.time(f1(data4x))
system.time(f1(data6x))
#Q2 - O(n)
system.time(f2(bs))
system.time(f2(data2x))
system.time(f2(data4x))
system.time(f2(data6x))
#Q3 - O(n)
system.time(f3(bs))
system.time(f3(data2x))
system.time(f3(data4x))
system.time(f3(data6x))
#Q4 - O(n^2)
system.time(f4(bs))
system.time(f4(data2x))
system.time(f4(data4x))
system.time(f4(data6x))

#The answer in question 5 matches the result in question 6 where time elapsed 
#increases linearly as the input size increased 
