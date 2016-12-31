# Working Directory
setwd("/Users/soorajshetty/R")

# Reading the table
AirDelay <- read.table("AirlineDelays.txt",sep = ",")

# Verify the table
head(AirDelay)

# Replacing NA by 0
AirDelay[is.na(AirDelay)]<-0

# Verify changes in the table
head(AirDelay)

# To factor in all delays, they have to be summed up in a new column
# Here, to perform this operation either 'for' or 'while' has to be implemented
# I have used 'while' to perform this operation

r <- 1; count <- 0				# 'r' is for the rows, which start with 1 till all the rows are counted
while (r < nrow(AirDelay)){
	if (AirDelay[r,7] > 0){count <- count + 1}
	if (AirDelay[r,9] > 0){count <- count + 1}
	if (AirDelay[r,10] > 0){count <- count + 1}
	if (AirDelay[r,11] > 0){count <- count + 1}
	if (AirDelay[r,12] > 0){count <- count + 1}
	if (AirDelay[r,13] > 0){count <- count + 1}

# Let the new column be named TotDel. Following operation puts the sum of all delays in TotDel
	AirDelay$TotDel[r] <- count
	count <- 0
	r <- r+1
}

# Verification of the modified table, which includes the newly created column, TotDel
head(AirDelay)

# TotalNumDelays function -
# Here, the grouping of total delay is done based on the carrier
# Aggregate function helps in performing this operation
a <- aggregate(TotDel~CARRIER,AirDelay,sum)	
head(a)							# Verify the grouping
# For ease of use, I am renaming my columns
colnames(a)<- cbind("CARRIER","DELAY")

TotalNumDelays<-function(Carrier){
# This function returns the delay for the corresponding carrier passed as the paramenter
	return(a$DELAY[which(a$CARRIER==Carrier)])
}

# Result of the function TotalNumDelays for 'DL'
TotalNumDelays("DL")

#TotalDelaysByOrigin function -
# Here, the grouping of total delay is done based on the origin
# Aggregate function helps in performing this operation
b <- aggregate(TotDel~ORIGIN,AirDelay,sum)
head(b)							# Verifying the grouping
# For ease of use, I am renaming my columns	
colnames(b) <- cbind("AIRPORT","DELAY")

TotalDelaysByOrigin <- function(Origin){
# This function returns the delay for the corresponding origin passed as the paramenter
	return(b$DELAY[which(b$AIRPORT==Origin)])
}

# Result of the function TotalDelaysByOrigin for 'ABQ'
TotalDelaysByOrigin("ABQ")

# AvgDelay function -
# Here, average arrival delay for a carrier flying into the specified destination airport
# New dataframe is created to just include carrier, destination & arrival delay
c <- data.frame(AirDelay$CARRIER,AirDelay$DEST,AirDelay$ARR_DELAY)
head(c)							# Verifying the dataframe

# Sorting arrival delay based on carrier & destination
d <- aggregate(c$AirDelay.ARR_DELAY, by=list(c$AirDelay.CARRIER, c$AirDelay.DEST),mean)
head(d)							# Verification

AvgDelay <- function(Carrier,Dest){
# The function returns average arrival delay based on carrier and destination passed
	return(d$x[which(d$Group.1==Carrier & d$Group.2==Dest)])
}	

# Result of the function AvgDelay for 'DL', 'ABQ'
AvgDelay("DL", "ABQ")
