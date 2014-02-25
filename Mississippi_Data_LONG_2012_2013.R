#############################################################################
###
### Syntax for preparation of Missississippi Long Data File
###
############################################################################

### Load SGP package

require(SGP)
require(data.table)


### Utility functions

strtail <- function(s,n=1) {
	if(n<0) substring(s,1-n)
	else substring(s,nchar(s)-n+1)
}
strhead <- function(s,n) {
	if(n<0) substr(s,1,nchar(s)+n)
	else substr(s,1,n)
}


### Load base data file

my.colClasses <- c(rep("character", 33))
Mississippi_Data_LONG_2012_2013 <- read.table("Data/Base_Files/SGP_Mississippi_2012_2013.txt", sep="|", header=TRUE, quote="", comment.char="", colClasses=my.colClasses)
load("Data/Base_Files/Pilot_SIG_District_Schools.Rdata")


### Remove Alternate assessment data

Mississippi_Data_LONG_2012_2013 <- subset(Mississippi_Data_LONG_2012_2013, MAAECF_MTH=="" & MAAECF_RLA=="" & MAAECF_SCI=="" & SCALE_SCORE!=0)


### Tidy up data

Mississippi_Data_LONG_2012_2013$CONTENT_AREA[Mississippi_Data_LONG_2012_2013$CONTENT_AREA=="READING LANGUAGE ARTS"] <- "READING_LANGUAGE_ARTS"
Mississippi_Data_LONG_2012_2013$CONTENT_AREA[Mississippi_Data_LONG_2012_2013$CONTENT_AREA=="US HISTORY"] <- "HISTORY"
Mississippi_Data_LONG_2012_2013$YEAR <- "2012_2013"
Mississippi_Data_LONG_2012_2013$GRADE <- as.character(as.integer(Mississippi_Data_LONG_2012_2013$GRADE))
Mississippi_Data_LONG_2012_2013$GRADE_ENROLLED <- Mississippi_Data_LONG_2012_2013$GRADE
Mississippi_Data_LONG_2012_2013$GRADE[Mississippi_Data_LONG_2012_2013$CONTENT_AREA %in% c("ALGEBRA", "BIOLOGY", "ENGLISH", "HISTORY")] <- "EOCT"
Mississippi_Data_LONG_2012_2013$VALID_CASE <- "VALID_CASE"
Mississippi_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL <- factor(Mississippi_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL, levels=1:4, labels=c("Minimal", "Basic", "Proficient", "Advanced"), ordered=TRUE)
Mississippi_Data_LONG_2012_2013$LAST_NAME <- as.factor(Mississippi_Data_LONG_2012_2013$LAST_NAME)
levels(Mississippi_Data_LONG_2012_2013$LAST_NAME) <- sapply(levels(Mississippi_Data_LONG_2012_2013$LAST_NAME), capwords)
Mississippi_Data_LONG_2012_2013$FIRST_NAME <- as.factor(Mississippi_Data_LONG_2012_2013$FIRST_NAME)
Mississippi_Data_LONG_2012_2013$LAST_NAME <- factor(as.character(Mississippi_Data_LONG_2012_2013$LAST_NAME))
levels(Mississippi_Data_LONG_2012_2013$FIRST_NAME) <- sapply(levels(Mississippi_Data_LONG_2012_2013$FIRST_NAME), capwords)
levels(Mississippi_Data_LONG_2012_2013$FIRST_NAME)[3] <- "Nicholas"
Mississippi_Data_LONG_2012_2013$FIRST_NAME <- factor(as.character(Mississippi_Data_LONG_2012_2013$FIRST_NAME))
Mississippi_Data_LONG_2012_2013$SCHOOL_NAME <- as.factor(Mississippi_Data_LONG_2012_2013$SCHOOL_NAME)
levels(Mississippi_Data_LONG_2012_2013$SCHOOL_NAME) <- sapply(levels(Mississippi_Data_LONG_2012_2013$SCHOOL_NAME), capwords)
Mississippi_Data_LONG_2012_2013$SCHOOL_NUMBER <- paste(Mississippi_Data_LONG_2012_2013$SCHOOL_NUMBER, "0", sep="")
Mississippi_Data_LONG_2012_2013$DISTRICT_NAME <- as.factor(Mississippi_Data_LONG_2012_2013$DISTRICT_NAME)
levels(Mississippi_Data_LONG_2012_2013$DISTRICT_NAME) <- sapply(levels(Mississippi_Data_LONG_2012_2013$DISTRICT_NAME), capwords)
Mississippi_Data_LONG_2012_2013$STATE_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled State: Yes", "Enrolled State: No"))
Mississippi_Data_LONG_2012_2013$DISTRICT_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled District: Yes", "Enrolled District: No"))
Mississippi_Data_LONG_2012_2013$SCHOOL_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled School: Yes", "Enrolled School: No"))
Mississippi_Data_LONG_2012_2013$GENDER <- as.factor(Mississippi_Data_LONG_2012_2013$GENDER)
levels(Mississippi_Data_LONG_2012_2013$GENDER) <- c("Female", "Male")
Mississippi_Data_LONG_2012_2013$ETHNICITY <- NA
Mississippi_Data_LONG_2012_2013$ETHNICITY <- as.factor(Mississippi_Data_LONG_2012_2013$ETHNICITY)
Mississippi_Data_LONG_2012_2013$FREE_REDUCED_LUNCH_STATUS[Mississippi_Data_LONG_2012_2013$FREE_REDUCED_LUNCH_STATUS==""] <- NA
Mississippi_Data_LONG_2012_2013$FREE_REDUCED_LUNCH_STATUS <- as.factor(Mississippi_Data_LONG_2012_2013$FREE_REDUCED_LUNCH_STATUS)
levels(Mississippi_Data_LONG_2012_2013$FREE_REDUCED_LUNCH_STATUS) <- c("Free Lunch", "Paid Lunch", "Reduced Price Lunch")
Mississippi_Data_LONG_2012_2013$IEP_STATUS <- as.factor(Mississippi_Data_LONG_2012_2013$IEP_STATUS)
levels(Mississippi_Data_LONG_2012_2013$IEP_STATUS) <- c("IEP: No", "IEP: Yes")
Mississippi_Data_LONG_2012_2013$ELL_STATUS <- as.factor(Mississippi_Data_LONG_2012_2013$ELL_STATUS)
levels(Mississippi_Data_LONG_2012_2013$ELL_STATUS) <- c("ELL: No", "ELL: Yes")
Mississippi_Data_LONG_2012_2013$SCALE_SCORE <- as.numeric(Mississippi_Data_LONG_2012_2013$SCALE_SCORE)
Mississippi_Data_LONG_2012_2013$TEST_ADMINISTRATION <- as.factor(Mississippi_Data_LONG_2012_2013$TEST_ADMINISTRATION)


Mississippi_Data_LONG_2012_2013$INSTRUCTOR_1_ENROLLMENT_STATUS <- NULL
Mississippi_Data_LONG_2012_2013$INSTRUCTOR_2_ENROLLMENT_STATUS <- NULL
#Mississippi_Data_LONG_2012_2013$INSTRUCTOR_NUMBER_1 <- NULL
Mississippi_Data_LONG_2012_2013$INSTRUCTOR_NUMBER_2 <- NULL
Mississippi_Data_LONG_2012_2013$INSTRUCTOR_1_WEIGHT <- NULL
Mississippi_Data_LONG_2012_2013$INSTRUCTOR_2_WEIGHT <- NULL
Mississippi_Data_LONG_2012_2013$GIFTED_AND_TALENTED <- NULL
Mississippi_Data_LONG_2012_2013$EMH_LEVEL <- NULL
Mississippi_Data_LONG_2012_2013$ETHNICITY <- NULL

# Merge in Pilot_SIG School identifier

Mississippi_Data_LONG_2012_2013 <- merge(Mississippi_Data_LONG_2012_2013, Pilot_SIG_District_Schools, all.x=TRUE)


###
### Invalidate cases
###

Mississippi_Data_LONG_2012_2013 <- as.data.table(Mississippi_Data_LONG_2012_2013)
setkeyv(Mississippi_Data_LONG_2012_2013, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID", "GRADE", "SCALE_SCORE"))
setkeyv(Mississippi_Data_LONG_2012_2013, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID"))
Mississippi_Data_LONG_2012_2013[which(duplicated(Mississippi_Data_LONG_2012_2013))-1, VALID_CASE := "INVALID_CASE"]
Mississippi_Data_LONG_2012_2013[GRADE %in% c("2", "56", "58", "64", "78"), VALID_CASE := "INVALID_CASE"]
Mississippi_Data_LONG_2012_2013 <- as.data.frame(Mississippi_Data_LONG_2012_2013)


### Tidy up order of variables

preferred.variable.order <- c("ID", "CONTENT_AREA", "TEST_ADMINISTRATION", "YEAR", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "LAST_NAME", "FIRST_NAME", "GRADE_ENROLLED",
				"SCHOOL_NUMBER", "SCHOOL_NAME", "DISTRICT_NUMBER", "DISTRICT_NAME", 
				"STATE_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS", "GENDER",
				"FREE_REDUCED_LUNCH_STATUS", "IEP_STATUS", "ELL_STATUS", "PILOT_SIG_SCHOOL", "VALID_CASE")

Mississippi_Data_LONG_2012_2013 <- Mississippi_Data_LONG_2012_2013[,preferred.variable.order]


### Save long file

save(Mississippi_Data_LONG_2012_2013, file="Data/Mississippi_Data_LONG_2012_2013.Rdata")
