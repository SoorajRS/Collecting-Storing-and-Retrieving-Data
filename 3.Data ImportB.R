library(XML)

B <- xmlToDataFrame('http://www.xmldatasets.net/temp/179681356453762.xml')

#senatorName function
#Function accepts name of the state & returns senators for the state
senatorName <- function(state) {
	return(B[which(B$state == state), c(2, 3)])
}

#Test
senatorName("MA")
#   last_name first_name
#13     Brown   Scott P.
#49     Kerry    John F.

#senatorPhone function
#Function accepts name of senator & returns the phone number of the senator
senatorPhone <- function(fullName){
	p <- which(paste(B$first_name, B$last_name) == fullName)
	paste(B$phone[p])
}

#Test
senatorPhone("Richard Burr")
#[1] "(202) 224-3154"
