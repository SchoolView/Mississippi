############################################################################################
###
### Construct Teacher data LONG file for 2012-2013
###
############################################################################################

### Load package

require(SGP)
require(data.table)


### Load Data

load("Data/Mississippi_Data_LONG_2012_2013.Rdata")
Instructor_Number_by_Content_Area_by_ID_2012_2013 <- as.data.table(read.table("Data/Base_Files/Teacher_Student_Content_Area_Lookup_2012_2013.txt", sep="|", header=TRUE, quote="", comment.char="", colClasses=rep("character", 4)))
Teacher_Data_2012_2013 <- data.table(read.table("Data/Base_Files/Teacher_ID_2012_2013.txt", sep="|", header=TRUE, quote="", comment.char="", colClasses=c("character", "factor", "factor")),
        key="INSTRUCTOR_NUMBER")



### Create LONG data set

setnames(Instructor_Number_by_Content_Area_by_ID_2012_2013, c("INSTRUCTOR_NUMBER", "CONTENT_AREA", "ID", "INSTRUCTOR_ENROLLMENT_STATUS"))

INSTRUCTOR_NUMBER <- data.table(
		Instructor_Number_by_Content_Area_by_ID_2012_2013,
		YEAR="2012_2013",
		INSTRUCTOR_WEIGHT=1L, key="INSTRUCTOR_NUMBER")

INSTRUCTOR_NUMBER[CONTENT_AREA=="READING LANGUAGE ARTS", CONTENT_AREA:="READING_LANGUAGE_ARTS"]
INSTRUCTOR_NUMBER <- subset(INSTRUCTOR_NUMBER, INSTRUCTOR_NUMBER!="" & INSTRUCTOR_NUMBER!="0")
setnames(Teacher_Data_2012_2013, c("INSTRUCTOR_NUMBER", "LAST_NAME", "FIRST_NAME"), c("INSTRUCTOR_NUMBER", "INSTRUCTOR_LAST_NAME", "INSTRUCTOR_FIRST_NAME")) 
INSTRUCTOR_NUMBER <- Teacher_Data_2012_2013[INSTRUCTOR_NUMBER]
INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME[INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME==""] <- NA
INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME[INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME==""] <- NA
INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME <- factor(INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME)
INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME <- factor(INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME)
INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME <- as.factor(INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME)
levels(INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME) <- sapply(levels(INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME), capwords)
INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME <- as.factor(INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME)
levels(INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME) <- sapply(levels(INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME), capwords)
setkey(INSTRUCTOR_NUMBER, ID, CONTENT_AREA, YEAR)
INSTRUCTOR_NUMBER <- subset(INSTRUCTOR_NUMBER, ID %in% Mississippi_Data_LONG_2012_2013$ID)
INSTRUCTOR_NUMBER$INSTRUCTOR_ENROLLMENT_STATUS <- factor(INSTRUCTOR_NUMBER$INSTRUCTOR_ENROLLMENT_STATUS, labels=c("Enrolled Instructor: No", "Enrolled Instructor: Yes"))


### Rename and save results

Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2012_2013 <- INSTRUCTOR_NUMBER
save(Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2012_2013, file="Data/Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2012_2013.Rdata")
