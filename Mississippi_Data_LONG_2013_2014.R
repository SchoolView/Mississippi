#############################################################################
###
### Syntax for preparation of Missississippi 2013-2014 Long Data File
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

my.colClasses <- c(rep("character", 47))
Mississippi_Data_WIDE_2013_2014 <- as.data.table(read.delim("Data/Base_Files/SGP_Mississippi_2013_2014.txt", colClasses=my.colClasses))
load("Data/Base_Files/Pilot_SIG_District_Schools.Rdata")
Pilot_SIG_District_Schools$DISTRICT_NAME <- Pilot_SIG_District_Schools$SCHOOL_NAME <- NULL

### Create LONG Data

tmp.variables.for.all <- c("MSIS", "SCHYR", "GRADE", "LAST", "FIRST", "SCHOOL_NUMBER", "NAME", "DISTRICT_NUMBER", "NAME_1", "GENDER", "RACE", "SPED", "ELL", "LEP")

## Grade Level Exam

TMP_CGE_Data <- Mississippi_Data_WIDE_2013_2014[,c(tmp.variables.for.all, "CGEMTHGRADE", "CGEMTHSCORE", "CGEMTHLEV", "CGERLAGRADE", "CGERLASCORE", "CGERLALEV", "CGESCIGRADE", "CGESCISCORE", "CGESCILEV"), with=FALSE]

TMP_CGE_Data$CGESCIGRADE[TMP_CGE_Data$CGESCIGRADE==" " & TMP_CGE_Data$GRADE %in% c("5", "8")] <- TMP_CGE_Data$GRADE[TMP_CGE_Data$CGESCIGRADE==" " & TMP_CGE_Data$GRADE %in% c("5", "8")]

attach(TMP_CGE_Data)
TMP_CGE_Data_LONG <- data.table(
			ID=strtail(paste("00000", rep(MSIS, 3), sep=""), 9),
			YEAR="2013_2014",
			TEST_ADMINISTRATION="MCT",
			CONTENT_AREA=rep(c("MATHEMATICS", "READING_LANGUAGE_ARTS", "SCIENCE"), each=dim(TMP_CGE_Data)[1]),
			GRADE=c(CGEMTHGRADE, CGERLAGRADE, CGESCIGRADE),
			GRADE_ENROLLED=rep(GRADE, 3),
			LAST_NAME=as.factor(rep(LAST, 3)),
			FIRST_NAME=as.factor(rep(FIRST, 3)),
			SCHOOL_NUMBER=strtail(paste("00", paste(rep(SCHOOL_NUMBER, ), "0", sep=""), sep=""), 4),
			SCHOOL_NAME=as.factor(rep(NAME, 3)),
			DISTRICT_NUMBER=strtail(paste("000", rep(DISTRICT_NUMBER, ), sep=""), 4),
			DISTRICT_NAME=as.factor(rep(NAME_1, 3)),
			GENDER=as.factor(rep(GENDER, 3)),
			ETHNICITY=as.factor(rep(RACE, 3)),
			IEP_STATUS=rep(SPED, 3),
			ELL_STATUS=rep(LEP, 3),
			SCALE_SCORE=as.numeric(c(CGEMTHSCORE, CGERLASCORE, CGESCISCORE)),
			ACHIEVEMENT_LEVEL=c(CGEMTHLEV, CGERLALEV, CGESCILEV))

			
TMP_CGE_Data_LONG <- subset(TMP_CGE_Data_LONG, GRADE!="" & GRADE %in% as.character(3:8)) 
TMP_CGE_Data_LONG$TEST_ADMINISTRATION[TMP_CGE_Data_LONG$CONTENT_AREA=="SCIENCE"] <- "SCI"
levels(TMP_CGE_Data_LONG$LAST_NAME) <- sapply(levels(TMP_CGE_Data_LONG$LAST_NAME), capwords)
levels(TMP_CGE_Data_LONG$FIRST_NAME) <- sapply(levels(TMP_CGE_Data_LONG$FIRST_NAME), capwords)
levels(TMP_CGE_Data_LONG$SCHOOL_NAME) <- sapply(levels(TMP_CGE_Data_LONG$SCHOOL_NAME), capwords)
levels(TMP_CGE_Data_LONG$DISTRICT_NAME) <- sapply(levels(TMP_CGE_Data_LONG$DISTRICT_NAME), capwords)
TMP_CGE_Data_LONG$SCHOOL_NUMBER <- paste(TMP_CGE_Data_LONG$DISTRICT_NUMBER, TMP_CGE_Data_LONG$SCHOOL_NUMBER, sep="")
levels(TMP_CGE_Data_LONG$GENDER) <- c("Female", "Male")
levels(TMP_CGE_Data_LONG$ETHNICITY) <- c("Asian", "African American", "Hispanic", "Pacific Islander", "Mixed Race", "White")
TMP_CGE_Data_LONG$IEP_STATUS[TMP_CGE_Data_LONG$IEP_STATUS==""] <- "N"
TMP_CGE_Data_LONG$IEP_STATUS <- as.factor(TMP_CGE_Data_LONG$IEP_STATUS)
levels(TMP_CGE_Data_LONG$IEP_STATUS) <- c("IEP: No", "IEP: Yes")
TMP_CGE_Data_LONG$ELL_STATUS[TMP_CGE_Data_LONG$ELL_STATUS==""] <- "N"
TMP_CGE_Data_LONG$ELL_STATUS <- as.factor(TMP_CGE_Data_LONG$ELL_STATUS)
levels(TMP_CGE_Data_LONG$ELL_STATUS) <- c("ELL: No", "ELL: Yes")
TMP_CGE_Data_LONG$ACHIEVEMENT_LEVEL[TMP_CGE_Data_LONG$ACHIEVEMENT_LEVEL==""] <- NA
TMP_CGE_Data_LONG$ACHIEVEMENT_LEVEL <- factor(TMP_CGE_Data_LONG$ACHIEVEMENT_LEVEL, levels=as.character(1:4), labels=c("Minimal", "Basic", "Proficient", "Advanced"), ordered=TRUE)
detach(TMP_CGE_Data)


## High School Exam

TMP_CHS_Data <- Mississippi_Data_WIDE_2013_2014[,c(tmp.variables.for.all, "CHSHISSCORE", "CHSHISLEV", "CHSMTHSCORE", "CHSMTHLEV", "CHSRLASCORE", "CHSRLALEV", "CHSSCISCORE", "CHSSCILEV", "CHSHISSCORE", "CHSHISLEV"), with=FALSE]

attach(TMP_CHS_Data)
TMP_CHS_Data_LONG <- data.table(
			ID=strtail(paste("00000", rep(MSIS, 4), sep=""), 9),
			YEAR="2013_2014",
			TEST_ADMINISTRATION="SATP",
			CONTENT_AREA=rep(c("ALGEBRA", "ENGLISH", "BIOLOGY", "HISTORY"), each=dim(TMP_CHS_Data)[1]),
			GRADE="EOCT",
			GRADE_ENROLLED=rep(GRADE, 4),
			LAST_NAME=as.factor(rep(LAST, 4)),
			FIRST_NAME=as.factor(rep(FIRST, 4)),
			SCHOOL_NUMBER=strtail(paste("000", paste(rep(SCHOOL_NUMBER, 4), "0", sep=""), sep=""), 4),
			SCHOOL_NAME=as.factor(rep(NAME, 4)),
			DISTRICT_NUMBER=strtail(paste("000", rep(DISTRICT_NUMBER, 4), sep=""), 4),
			DISTRICT_NAME=as.factor(rep(NAME_1, 4)),
			GENDER=as.factor(rep(GENDER, 4)),
			ETHNICITY=as.factor(rep(RACE, 4)),
			IEP_STATUS=rep(SPED, 4),
			ELL_STATUS=rep(LEP, 4),
			SCALE_SCORE=as.numeric(c(CHSMTHSCORE, CHSRLASCORE, CHSSCISCORE, CHSHISSCORE)),
			ACHIEVEMENT_LEVEL=c(CHSMTHLEV, CHSRLALEV, CHSSCILEV, CHSHISLEV))

			
TMP_CHS_Data_LONG <- subset(TMP_CHS_Data_LONG, !is.na(SCALE_SCORE)) 
levels(TMP_CHS_Data_LONG$LAST_NAME) <- sapply(levels(TMP_CHS_Data_LONG$LAST_NAME), capwords)
levels(TMP_CHS_Data_LONG$FIRST_NAME) <- sapply(levels(TMP_CHS_Data_LONG$FIRST_NAME), capwords)
levels(TMP_CHS_Data_LONG$SCHOOL_NAME) <- sapply(levels(TMP_CHS_Data_LONG$SCHOOL_NAME), capwords)
levels(TMP_CHS_Data_LONG$DISTRICT_NAME) <- sapply(levels(TMP_CHS_Data_LONG$DISTRICT_NAME), capwords)
TMP_CHS_Data_LONG$SCHOOL_NUMBER <- paste(TMP_CHS_Data_LONG$DISTRICT_NUMBER, TMP_CHS_Data_LONG$SCHOOL_NUMBER, sep="")
levels(TMP_CHS_Data_LONG$GENDER) <- c("Female", "Male")
levels(TMP_CHS_Data_LONG$ETHNICITY) <- c("Asian", "African American", "Hispanic", "Pacific Islander", "Mixed Race", "White")
TMP_CHS_Data_LONG$IEP_STATUS[TMP_CHS_Data_LONG$IEP_STATUS==""] <- "N"
TMP_CHS_Data_LONG$IEP_STATUS <- as.factor(TMP_CHS_Data_LONG$IEP_STATUS)
levels(TMP_CHS_Data_LONG$IEP_STATUS) <- c("IEP: No", "IEP: Yes")
TMP_CHS_Data_LONG$ELL_STATUS[TMP_CHS_Data_LONG$ELL_STATUS==""] <- "N"
TMP_CHS_Data_LONG$ELL_STATUS <- as.factor(TMP_CHS_Data_LONG$ELL_STATUS)
levels(TMP_CHS_Data_LONG$ELL_STATUS) <- c("ELL: No", "ELL: Yes")
TMP_CHS_Data_LONG$ACHIEVEMENT_LEVEL[TMP_CHS_Data_LONG$ACHIEVEMENT_LEVEL==""] <- NA
TMP_CHS_Data_LONG$ACHIEVEMENT_LEVEL <- factor(TMP_CHS_Data_LONG$ACHIEVEMENT_LEVEL, levels=as.character(1:4), labels=c("Minimal", "Basic", "Proficient", "Advanced"), ordered=TRUE)
detach(TMP_CHS_Data)


### Stack Data

Mississippi_Data_LONG_2013_2014 <- rbind.fill(TMP_CGE_Data_LONG, TMP_CHS_Data_LONG)
Mississippi_Data_LONG_2013_2014$TEST_ADMINISTRATION <- as.factor(Mississippi_Data_LONG_2013_2014$TEST_ADMINISTRATION)


# Merge in Pilot_SIG School identifier

Mississippi_Data_LONG_2013_2014 <- merge(Mississippi_Data_LONG_2013_2014, Pilot_SIG_District_Schools, all.x=TRUE)


###
### Invalidate cases
###

Mississippi_Data_LONG_2013_2014 <- as.data.table(Mississippi_Data_LONG_2013_2014)
Mississippi_Data_LONG_2013_2014[,VALID_CASE:="VALID_CASE"]
Mississippi_Data_LONG_2013_2014[,STATE_ENROLLMENT_STATUS:="Enrolled State: Yes"]
Mississippi_Data_LONG_2013_2014[,DISTRICT_ENROLLMENT_STATUS:="Enrolled District: Yes"]
Mississippi_Data_LONG_2013_2014[,SCHOOL_ENROLLMENT_STATUS:="Enrolled School: Yes"]
setkeyv(Mississippi_Data_LONG_2013_2014, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID", "GRADE", "SCALE_SCORE"))
setkeyv(Mississippi_Data_LONG_2013_2014, c("VALID_CASE", "YEAR", "CONTENT_AREA", "ID"))
Mississippi_Data_LONG_2013_2014[which(duplicated(Mississippi_Data_LONG_2013_2014))-1, VALID_CASE := "INVALID_CASE"]


### Tidy up order of variables

preferred.variable.order <- c("ID", "CONTENT_AREA", "TEST_ADMINISTRATION", "YEAR", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "LAST_NAME", "FIRST_NAME", "GRADE_ENROLLED",
				"SCHOOL_NUMBER", "SCHOOL_NAME", "DISTRICT_NUMBER", "DISTRICT_NAME", 
				"STATE_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS", "GENDER", "ETHNICITY",
				"IEP_STATUS", "ELL_STATUS", "PILOT_SIG_SCHOOL", "VALID_CASE")

setcolorder(Mississippi_Data_LONG_2013_2014, preferred.variable.order)


### Save long file

save(Mississippi_Data_LONG_2013_2014, file="Data/Mississippi_Data_LONG_2013_2014.Rdata")
