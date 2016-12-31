library(openxlsx)
library(utils)
library(stringi)
library(reshape2)
require(openxlsx)
require(lubridate)

#1.Loading the data file
A <- read.xlsx("2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx", startRow=3)
head(A)

#2.Standardizing Seasons
dates <- A$Season1Date
head(dates)

View(as.data.frame(dates))
splitDates <- strsplit(dates, split = ' to')
splitDates <- as.data.frame(splitDates)
splitDates <- t(splitDates)
row.names(splitDates) <- NULL
splitDates <- as.data.frame(splitDates)
splitDates

A$Season1Date <- strsplit(A$Season1Date, split = " to ")
A <- A[!is.na(A["Season1Date"]),]

# use these to determine which start months qualify which seasons
winter <- c(month.name[12], month.name[1], month.name[2])
spring <- c(month.name[3], month.name[4], month.name[5])
summer <- c(month.name[6], month.name[7], month.name[8])

subtMths <- function(m1, m2) {
 return(match(m1, month.name) - match(m2, month.name))
}

getSesnFrmMth <- function(month) {
	ifelse(!is.na(match(month, winter)),
		return('Winter'),
		return(ifelse(!is.na(match(month, spring)),
			'Spring',
			ifelse(!is.na(match(month, summer)),
				'Summer',
				'Fall'))))
}

range&normalize <- function(dates) {
	split <- colsplit(string=dates, pattern=" to ", names=c('start', 'end'))
	split$start <- ifelse(
		(is.na(as.Date(split$start, format='%m/%d/%Y')) & is.na(as.Date(split$start, format='%B %d, %Y'))),
		split$start,
		ifelse(is.na(as.Date(split$start, format='%m/%d/%Y')),
			month.name[month(as.Date(split$start, format='%B %d, %Y'))],
			month.name[month(as.Date(split$start, format='%m/%d/%Y'))])
		)
		split$end <- ifelse(
			(is.na(as.Date(split$end, format='%m/%d/%Y')) & is.na(as.Date(split$end, format='%B %d, %Y'))),
			split$end,
			ifelse(is.na(as.Date(split$end, format='%m/%d/%Y')),
				month.name[month(as.Date(split$end, format='%B %d, %Y'))],
				month.name[month(as.Date(split$end, format='%m/%d/%Y'))])
		)
		split$range <- subtractMonths(split[,2], split[,1]) + 1
		split$normalized <- ifelse(is.na(split$range),
			NA,
			ifelse(split$range >= 10,
				'Year-Round',
				ifelse(split$range >= 6,
					'Half-Year',
					getSesnFrmMth(split$start))))
		return(split$normalized)
}

standardizeDates <- function() {
	A$Season1Normalized <<- range&normalize(A$Season1Date)
}
 
 
#3.Retrieval function shows which markets accept WIC
acceptsWIC <- function() {
	return(A[which(!is.na(A$WIC) & A$WIC == 'Y'),])
}
#Test case
acceptsWIC()
