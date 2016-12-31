library(R.utils)
library(stringr)

#1.Loading the data
D <- file("movies.list.gz")
open(D)
raw.text <- readLines(D)
close(D)

#2.Parsed data
lines <- data.frame(raw.text,stringsAsFactors=FALSE)
dim(lines)
#First 100 lines for sample
head(lines,100)
#Creating a smaller set of data
d <- head(lines,999999)

#3.Movie identification [Comments]
#Study of the data shows that movies do not start with special characters, for e.g. !,@,$,#,@,%,etc.
#Hence, it can be concluded that, movies start with charcters or numbers, for e.g. The Love Parade (1929) or Furious 7 (2015)
#However, I found other entries in the data for video games and such, for e.g. Assassin's Creed: Unity (2014)
#Like movies they do not have special characters, hence I was looked further into the data.
#Turns out video games have another classification as (VG)
#TV shows also posses this identifier, (TV)

#4.Only movies in the result
nonspecial <- str_extract(d,"[0-9a-zA-Z].+")
yr <- str_extract(d,"[0-9]{4}$")
#Dataframe devoid of lines starting with special characters
ny <- data.frame(nonspecial,yr)
#Removing unnecessay entries
#Removes tv show from data
ny1 <- ny[!grepl("\\(TV)", ny)]
#Removes video games from data
ny2 <- ny1[!grepl("\\(VG)", ny1)]
#Removes tabs for cleaner data
mov <- gsub("[\t]", '', ny2)

#Sample data
head(mov)
