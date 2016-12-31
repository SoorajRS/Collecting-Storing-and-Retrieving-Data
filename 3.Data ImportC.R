library(XML)

#Question 1
C <- xmlTreeParse("http://www.cs.washington.edu/research/xmldatasets/data/auctions/ebay.xml")
R <- xmlRoot(C)
xmlSize(R)

#Function to find auctions had more than 5 bids
moreFiveBids <- function(R){
	 count <-  0
	 for(i in 1:xmlSize(R)){
	 	if(as.integer(xmlValue(R[[i]][[5]][[5]])) > 5){
	 		count <-  count + 1
	 	}
	 }
	 paste(count,"auctions had more than 5 bids.")
}
#Test case
moreFiveBids(R)
#"5 auctions had more than 5 bids."

#Question 2
rm(list=ls())
C2 <- xmlTreeParse("http://www.barchartmarketdata.com/data-samples/getHistory15.xml")
R2 <- xmlRoot(C2)
xmlSize(R2)

#2A - highest closing price for security
highestClosingPrice <- function(R2){
	close <- c()
	for(i in 2:xmlSize(R2)){
		close[i-1] <- (as.numeric(xmlValue(R2[[i]][[7]])))
	}
	maxClose <- which.max(close)
	paste(close[maxClose], "was the highest closing price for the security.")
}
#Test case
highestClosingPrice(R2)
#"1768.5 was the highest closing price for the security."

#2B - total volume traded
totalVolume <- function(R2){
	total <- 0
	for(i in 2:xmlSize(R2)){
		total <- total + (as.integer(xmlValue(R2[[i]][[8]])))
	}
	paste(total, "was the total volume traded")
}
#Test case
totalVolume(R2)
#"1177000 was the total volume traded"

#2C - average trading volume during each hour of trading day
C3 <- xmlToDataFrame("http://www.barchartmarketdata.com/data-samples/getHistory15.xml")
#Cleaning dataframe
R3 <- C3[2:nrow(C3), 3:ncol(C3)]
#Parsing timestamp into date and time format
dateTime <- strptime(R3$timestamp, "%Y-%m-%dT%H:%M:%OS")
#Adding column having the date and time to the dataframe
R3$dateTime <- dateTime
#Final dataframe that to be used for analysis
final <- as.data.frame(R3)
#Function to find the average trading volume during each hour
averageVolume <- function(final){
	#aggregate slices the dataframe by the hours of the day
	avgVolume <- as.data.frame(aggregate(x = as.numeric(R3$volume),by = (hour=list(R3$hour)),mean))
	colnames(avgVolume) <- c("Hour", "Avg Volume")
	avgVolume
}
#Test case
averageVolume(final)
