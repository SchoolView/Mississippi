#############################################################################
###
### Syntax for preparation of Mississippi Long Data File
###
############################################################################

### Load SGP package

require(SGP)


### Load base data file

my.colClasses <- c(rep("factor", 5), "numeric", "character", rep("factor", 3), rep("integer", 2), rep("factor", 4), rep("numeric", 2), rep("factor", 12))
Mississippi_Data_LONG <- read.table("Data/Base_Files/SGP_Mississippi.csv", sep="|", header=TRUE, quote="", comment.char="", colClasses=my.colClasses)
Pilot_School <- load("Data/Base_Files/Pilot_District_Schools.Rdata")
SIG_School <- load("Data/Base_Files/SIG_District_Schools.Rdata")
Teacher_Data <- read.table("Data/Base_Files/Teacher_ID_2011_2012.csv", sep="|", header=TRUE, quote="")

# Create Pilot and SIG school lookup table

Pilot_School$PILOT_SIG_SCHOOL <- factor(1, levels=1:2, labels=c("Pilot School", "SIG School"))

SIG_School$PILOT_SIG_SCHOOL <- factor(2, levels=1:2, labels=c("Pilot School", "SIG School"))

Pilot_SIG_School <- rbind(Pilot_School, SIG_School)


# Merge in Pilot_SIG School identifier

Mississippi_Data_LONG <- merge(Mississippi_Data_LONG, Pilot_SIG_School, all.x=TRUE)

Teacher_Data <- Teacher_Data[,1:3]
names(Teacher_Data)[1:3] <- c("INSTRUCTOR_NUMBER_1", "INSTRUCTOR_1_LAST_NAME", "INSTRUCTOR_1_FIRST_NAME")
Mississippi_Data_LONG <- merge(Mississippi_Data_LONG, Teacher_Data, all.x=TRUE)
names(Teacher_Data)[1:3] <- c("INSTRUCTOR_NUMBER_2", "INSTRUCTOR_2_LAST_NAME", "INSTRUCTOR_2_FIRST_NAME")
Mississippi_Data_LONG <- merge(Mississippi_Data_LONG, Teacher_Data, all.x=TRUE)


### Tidy up data

Mississippi_Data_LONG$ID <- as.character(Mississippi_Data_LONG$ID)
levels(Mississippi_Data_LONG$CONTENT_AREA)[5] <- "READING_LANGUAGE_ARTS"
Mississippi_Data_LONG$CONTENT_AREA <- as.character(Mississippi_Data_LONG$CONTENT_AREA)
levels(Mississippi_Data_LONG$YEAR) <- "2011_2012"
Mississippi_Data_LONG$YEAR <- as.character(Mississippi_Data_LONG$YEAR)
Mississippi_Data_LONG$GRADE <- as.integer(as.character(Mississippi_Data_LONG$GRADE))
Mississippi_Data_LONG$VALID_CASE <- factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE"))
Mississippi_Data_LONG$VALID_CASE <- as.character(Mississippi_Data_LONG$VALID_CASE)
levels(Mississippi_Data_LONG$ACHIEVEMENT_LEVEL) <- c("Minimal", "Basic", "Proficient", "Advanced")
Mississippi_Data_LONG$ACHIEVEMENT_LEVEL <- as.ordered(Mississippi_Data_LONG$ACHIEVEMENT_LEVEL)
levels(Mississippi_Data_LONG$LAST_NAME) <- sapply(levels(Mississippi_Data_LONG$LAST_NAME), capwords)
levels(Mississippi_Data_LONG$FIRST_NAME) <- sapply(levels(Mississippi_Data_LONG$FIRST_NAME), capwords)
levels(Mississippi_Data_LONG$INSTRUCTOR_1_LAST_NAME) <- sapply(levels(Mississippi_Data_LONG$INSTRUCTOR_1_LAST_NAME), capwords)
levels(Mississippi_Data_LONG$INSTRUCTOR_2_LAST_NAME) <- sapply(levels(Mississippi_Data_LONG$INSTRUCTOR_2_LAST_NAME), capwords)
levels(Mississippi_Data_LONG$INSTRUCTOR_1_FIRST_NAME) <- sapply(levels(Mississippi_Data_LONG$INSTRUCTOR_1_FIRST_NAME), capwords)
levels(Mississippi_Data_LONG$INSTRUCTOR_2_FIRST_NAME) <- sapply(levels(Mississippi_Data_LONG$INSTRUCTOR_2_FIRST_NAME), capwords)
Mississippi_Data_LONG$INSTRUCTOR_1_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled Instructor: Yes", "Enrolled Instructor: No"))
Mississippi_Data_LONG$INSTRUCTOR_2_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled Instructor: Yes", "Enrolled Instructor: No"))
Mississippi_Data_LONG$STATE_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled State: Yes", "Enrolled State: No"))
Mississippi_Data_LONG$DISTRICT_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled District: Yes", "Enrolled District: No"))
Mississippi_Data_LONG$SCHOOL_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled School: Yes", "Enrolled School: No"))
levels(Mississippi_Data_LONG$GENDER) <- c("Female", "Male")
levels(Mississippi_Data_LONG$ETHNICITY) <- c("Asian", "African American", "Hispanic", "Mixed Race", "No Response", "Pacific Islander", "White")
Mississippi_Data_LONG$FREE_REDUCED_LUNCH_STATUS[Mississippi_Data_LONG$FREE_REDUCED_LUNCH_STATUS==""] <- NA
Mississippi_Data_LONG$FREE_REDUCED_LUNCH_STATUS <- factor(Mississippi_Data_LONG$FREE_REDUCED_LUNCH_STATUS)
levels(Mississippi_Data_LONG$FREE_REDUCED_LUNCH_STATUS) <- c("Free Lunch", "Paid Lunch", "Reduced Price Lunch")
levels(Mississippi_Data_LONG$IEP_STATUS) <- c("IEP: No", "IEP: Yes")
levels(Mississippi_Data_LONG$ELL_STATUS) <- c("ELL: No", "ELL: Yes")
Mississippi_Data_LONG$INSTRUCTOR_1_WEIGHT[!is.na(Mississippi_Data_LONG$INSTRUCTOR_NUMBER_1)] <- 1L
Mississippi_Data_LONG$INSTRUCTOR_2_WEIGHT[!is.na(Mississippi_Data_LONG$INSTRUCTOR_NUMBER_2)] <- 1L
Mississippi_Data_LONG$GIFTED_AND_TALENTED <- NULL
Mississippi_Data_LONG$EMH_LEVEL <- NULL


###
### Invalidate cases
###

Mississippi_Data_LONG$VALID_CASE[Mississippi_Data_LONG$CONTENT_AREA=="BIOLOGY" & Mississippi_Data_LONG$YEAR=="2011_2012" & Mississippi_Data_LONG$SCALE_SCORE < 500] <- "INVALID_CASE"
Mississippi_Data_LONG <- as.data.table(Mississippi_Data_LONG)
setkeyv(Mississippi_Data_LONG, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID", "GRADE", "SCALE_SCORE"))
setkeyv(Mississippi_Data_LONG, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID"))
invisible(Mississippi_Data_LONG[which(duplicated(Mississippi_Data_LONG))-1, VALID_CASE := "INVALID_CASE"])
Mississippi_Data_LONG <- as.data.frame(Mississippi_Data_LONG)


### Tidy up order of variables

preferred.variable.order <- c("ID", "CONTENT_AREA", "TEST_ADMINISTRATION", "YEAR", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "LAST_NAME", "FIRST_NAME",
				"SCHOOL_NUMBER", "SCHOOL_NAME", "DISTRICT_NUMBER", "DISTRICT_NAME", 
				"INSTRUCTOR_NUMBER_2", "INSTRUCTOR_NUMBER_1", "INSTRUCTOR_1_LAST_NAME", "INSTRUCTOR_1_FIRST_NAME", "INSTRUCTOR_2_LAST_NAME", "INSTRUCTOR_2_FIRST_NAME",
				"INSTRUCTOR_1_WEIGHT", "INSTRUCTOR_2_WEIGHT", "INSTRUCTOR_1_ENROLLMENT_STATUS", "INSTRUCTOR_2_ENROLLMENT_STATUS",
				"STATE_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS", "GENDER", "ETHNICITY",
				"FREE_REDUCED_LUNCH_STATUS", "IEP_STATUS", "ELL_STATUS", "PILOT_SIG_SCHOOL", "VALID_CASE")

Mississippi_Data_LONG <- Mississippi_Data_LONG[,preferred.variable.order]


### Save long file

#save(Mississippi_Data_LONG, file="Data/Mississippi_Data_LONG.Rdata")
