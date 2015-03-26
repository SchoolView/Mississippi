############################################################################################
###
### Construct Teacher data LONG file for 2013-2014
###
############################################################################################

### Load package

require(SGP)
require(data.table)


### Utility functions

strtail <- function (s, n = 1) {
    if (n < 0) 
        substring(s, 1 - n)
    else substring(s, nchar(s) - n + 1)
}


### Load Data

load("Data/Mississippi_Data_LONG_2013_2014.Rdata")
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014 <- fread("Data/Base_Files/Teacher_Student_Content_Area_Lookup_2013_2014_PART.txt")



### Tidy up data

Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,INSTRUCTOR_NUMBER:=as.character(INSTRUCTOR_NUMBER)]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,INSTRUCTOR_LAST_NAME:=as.factor(INSTRUCTOR_LAST_NAME)]
levels(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014$INSTRUCTOR_LAST_NAME) <- sapply(levels(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014$INSTRUCTOR_LAST_NAME), capwords)
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,INSTRUCTOR_FIRST_NAME:=as.factor(INSTRUCTOR_FIRST_NAME)]
levels(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014$INSTRUCTOR_FIRST_NAME) <- sapply(levels(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014$INSTRUCTOR_FIRST_NAME), capwords)
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,COURSE_NAME:=NULL]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,CONTENT_AREA:=toupper(CONTENT_AREA)]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[CONTENT_AREA=="ENGLISH LANGUAGE ARTS", CONTENT_AREA:="READING_LANGUAGE_ARTS"]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,ID:=strtail(paste("00000000", ID, sep=""), 9)]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,INSTRUCTOR_WEIGHT:=1L]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,YEAR:="2013_2014"]
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014[,INSTRUCTOR_ENROLLMENT_STATUS:=factor(1, levels=0:1, labels=c("Enrolled Instructor: No", "Enrolled Instructor: Yes"))]


### Remove duplicates

setkey(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014, INSTRUCTOR_NUMBER, CONTENT_AREA, ID)
Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014 <- unique(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014)


### Set column order and key

setcolorder(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014, 
	c("INSTRUCTOR_NUMBER", "INSTRUCTOR_LAST_NAME", "INSTRUCTOR_FIRST_NAME", "CONTENT_AREA", "ID", "INSTRUCTOR_ENROLLMENT_STATUS", "YEAR", "INSTRUCTOR_WEIGHT"))
setkey(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014, ID, CONTENT_AREA, YEAR)


### Save results

save(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014, file="Data/Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2013_2014.Rdata")
