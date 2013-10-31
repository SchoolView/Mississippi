###################################################################################
###
### Script to generate Mississippi bubblePlots for 2012-2013 data
###
###################################################################################

### Load SGP package

require(SGP)
require(data.table)
options(error=recover)


### Load data

load("Data/Mississippi_SGP.Rdata")
load("Data/Base_Files/Pilot_District_Schools.Rdata")
load("Data/Base_Files/SIG_District_Schools.Rdata")


### Define relevant quantities

MS_content_areas <- c("ALGEBRA", "BIOLOGY", "ENGLISH", "MATHEMATICS", "READING_LANGUAGE_ARTS")
MS_content_areas_test_type <- c("SATP", "SATP", "SATP", "SATP", "MCT2", "MCT2")


### Create data

## School

bPlot.data.school <- Mississippi_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
bPlot.data.school.2y <- Mississippi_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__SCHOOL_ENROLLMENT_STATUS"]][SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
bPlot.data.school.2y <- bPlot.data.school.2y[,list(SCHOOL_NUMBER, CONTENT_AREA, MEDIAN_SGP, MEDIAN_SGP_COUNT)]
setnames(bPlot.data.school.2y, 3:4, c("MEDIAN_SGP_2_YEAR", "MEDIAN_SGP_COUNT_2_YEAR"))


## School grade

bPlot.data.school.grade <- Mississippi_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__GRADE__SCHOOL_ENROLLMENT_STATUS"]][SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
bPlot.data.school.grade.2y <- Mississippi_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__GRADE__SCHOOL_ENROLLMENT_STATUS"]][SCHOOL_ENROLLMENT_STATUS=="Enrolled School: Yes"]
bPlot.data.school.grade.2y <- bPlot.data.school.grade.2y[,list(SCHOOL_NUMBER, CONTENT_AREA, GRADE, MEDIAN_SGP, MEDIAN_SGP_COUNT)]
setnames(bPlot.data.school.grade.2y, 4:5, c("MEDIAN_SGP_2_YEAR", "MEDIAN_SGP_COUNT_2_YEAR"))

## Instructor

bPlot.data.instructor <- Mississippi_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__YEAR__INSTRUCTOR_ENROLLMENT_STATUS"]][INSTRUCTOR_ENROLLMENT_STATUS=="Enrolled Instructor: Yes"]
bPlot.data.instructor.2y <- Mississippi_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__INSTRUCTOR_ENROLLMENT_STATUS"]][INSTRUCTOR_ENROLLMENT_STATUS=="Enrolled Instructor: Yes"]
bPlot.data.instructor.2y <- bPlot.data.instructor.2y[,list(SCHOOL_NUMBER, INSTRUCTOR_NUMBER, CONTENT_AREA, MEDIAN_SGP, MEDIAN_SGP_COUNT)]
setnames(bPlot.data.instructor.2y, 4:5, c("MEDIAN_SGP_2_YEAR", "MEDIAN_SGP_COUNT_2_YEAR"))


## Instructor grade 

bPlot.data.instructor.grade <- Mississippi_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__YEAR__GRADE__INSTRUCTOR_ENROLLMENT_STATUS"]][INSTRUCTOR_ENROLLMENT_STATUS=="Enrolled Instructor: Yes"]
bPlot.data.instructor.grade.2y <- Mississippi_SGP@Summary$SCHOOL_NUMBER[["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__GRADE__INSTRUCTOR_ENROLLMENT_STATUS"]][INSTRUCTOR_ENROLLMENT_STATUS=="Enrolled Instructor: Yes"]
bPlot.data.instructor.grade.2y <- bPlot.data.instructor.grade.2y[,list(SCHOOL_NUMBER, INSTRUCTOR_NUMBER, CONTENT_AREA, GRADE, MEDIAN_SGP, MEDIAN_SGP_COUNT)]
setnames(bPlot.data.instructor.grade.2y, 5:6, c("MEDIAN_SGP_2_YEAR", "MEDIAN_SGP_COUNT_2_YEAR"))


### Merge in School and District Names

setkeyv(Mississippi_SGP@Data, c("YEAR", "SCHOOL_NUMBER", "DISTRICT_NUMBER"))
tmp.school.and.district <- unique(Mississippi_SGP@Data)[,list(YEAR, SCHOOL_NUMBER, SCHOOL_NAME, DISTRICT_NUMBER, DISTRICT_NAME)]

setkeyv(tmp.school.and.district, c("SCHOOL_NUMBER", "YEAR"))
setkeyv(bPlot.data.school, c("SCHOOL_NUMBER", "YEAR"))
setkeyv(bPlot.data.school.grade, c("SCHOOL_NUMBER", "YEAR"))
setkeyv(bPlot.data.instructor, c("SCHOOL_NUMBER", "YEAR"))
setkeyv(bPlot.data.instructor.grade, c("SCHOOL_NUMBER", "YEAR"))

bPlot.data.school <- tmp.school.and.district[bPlot.data.school]
bPlot.data.school.grade <- tmp.school.and.district[bPlot.data.school.grade]
bPlot.data.instructor <- tmp.school.and.district[bPlot.data.instructor]
bPlot.data.instructor.grade <- tmp.school.and.district[bPlot.data.instructor.grade]


### Merge in Instructor names

tmp.dt.long <- data.table(
		YEAR=Mississippi_SGP@Data_Supplementary$INSTRUCTOR_NUMBER$YEAR,
		INSTRUCTOR_NUMBER=Mississippi_SGP@Data_Supplementary$INSTRUCTOR_NUMBER$INSTRUCTOR_NUMBER,
		INSTRUCTOR_FIRST_NAME=Mississippi_SGP@Data_Supplementary$INSTRUCTOR_NUMBER$INSTRUCTOR_FIRST_NAME,
		INSTRUCTOR_LAST_NAME=Mississippi_SGP@Data_Supplementary$INSTRUCTOR_NUMBER$INSTRUCTOR_LAST_NAME
	)

setkeyv(tmp.dt.long, c("INSTRUCTOR_NUMBER", "YEAR"))
tmp.instructor <- unique(tmp.dt.long)[,list(YEAR, INSTRUCTOR_NUMBER, INSTRUCTOR_FIRST_NAME, INSTRUCTOR_LAST_NAME)]
setkeyv(tmp.instructor, c("INSTRUCTOR_NUMBER", "YEAR"))
setkeyv(bPlot.data.instructor, c("INSTRUCTOR_NUMBER", "YEAR"))
setkeyv(bPlot.data.instructor.grade, c("INSTRUCTOR_NUMBER", "YEAR"))

bPlot.data.instructor <- tmp.instructor[bPlot.data.instructor]
bPlot.data.instructor.grade <- tmp.instructor[bPlot.data.instructor.grade]


### Merge 2 year and 1 year results

setkeyv(bPlot.data.school.2y, c("SCHOOL_NUMBER", "CONTENT_AREA"))
setkeyv(bPlot.data.school, c("SCHOOL_NUMBER", "CONTENT_AREA"))

bPlot.data.school <- bPlot.data.school.2y[bPlot.data.school]
bPlot.data.school <- subset(bPlot.data.school, YEAR=="2012_2013")

setkeyv(bPlot.data.school.grade.2y, c("SCHOOL_NUMBER", "CONTENT_AREA", "GRADE"))
setkeyv(bPlot.data.school.grade, c("SCHOOL_NUMBER", "CONTENT_AREA", "GRADE"))

bPlot.data.school.grade <- bPlot.data.school.grade.2y[bPlot.data.school.grade]
bPlot.data.school.grade <- subset(bPlot.data.school.grade, YEAR=="2012_2013")

setkeyv(bPlot.data.instructor.2y, c("SCHOOL_NUMBER", "INSTRUCTOR_NUMBER", "CONTENT_AREA"))
setkeyv(bPlot.data.instructor, c("SCHOOL_NUMBER", "INSTRUCTOR_NUMBER", "CONTENT_AREA"))

bPlot.data.instructor <- bPlot.data.instructor.2y[bPlot.data.instructor]
bPlot.data.instructor <- subset(bPlot.data.instructor, YEAR=="2012_2013")

setkeyv(bPlot.data.instructor.grade.2y, c("SCHOOL_NUMBER", "INSTRUCTOR_NUMBER", "CONTENT_AREA", "GRADE"))
setkeyv(bPlot.data.instructor.grade, c("SCHOOL_NUMBER", "INSTRUCTOR_NUMBER", "CONTENT_AREA", "GRADE"))

bPlot.data.instructor.grade <- bPlot.data.instructor.grade.2y[bPlot.data.instructor.grade]
bPlot.data.instructor.grade <- subset(bPlot.data.instructor.grade, YEAR=="2012_2013")

bPlot.data.school$PILOT_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("Pilot District School: No", "Pilot District School: Yes"))
bPlot.data.school.grade$PILOT_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("Pilot District School: No", "Pilot District School: Yes"))
bPlot.data.instructor$PILOT_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("Pilot District School: No", "Pilot District School: Yes"))
bPlot.data.instructor.grade$PILOT_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("Pilot District School: No", "Pilot District School: Yes"))
bPlot.data.school$PILOT_DISTRICT_SCHOOL[bPlot.data.school$SCHOOL_NUMBER %in% Pilot_District_Schools$SCHOOL_NUMBER] <- "Pilot District School: Yes"
bPlot.data.school.grade$PILOT_DISTRICT_SCHOOL[bPlot.data.school.grade$SCHOOL_NUMBER %in% Pilot_District_Schools$SCHOOL_NUMBER] <- "Pilot District School: Yes"
bPlot.data.instructor$PILOT_DISTRICT_SCHOOL[bPlot.data.instructor$SCHOOL_NUMBER %in% Pilot_District_Schools$SCHOOL_NUMBER] <- "Pilot District School: Yes"
bPlot.data.instructor.grade$PILOT_DISTRICT_SCHOOL[bPlot.data.instructor.grade$SCHOOL_NUMBER %in% Pilot_District_Schools$SCHOOL_NUMBER] <- "Pilot District School: Yes"

bPlot.data.school$SIG_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("SIG District School: No", "SIG District School: Yes"))
bPlot.data.school.grade$SIG_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("SIG District School: No", "SIG District School: Yes"))
bPlot.data.instructor$SIG_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("SIG District School: No", "SIG District School: Yes"))
bPlot.data.instructor.grade$SIG_DISTRICT_SCHOOL <- factor(1, levels=1:2, labels=c("SIG District School: No", "SIG District School: Yes"))
bPlot.data.school$SIG_DISTRICT_SCHOOL[bPlot.data.school$SCHOOL_NUMBER %in% SIG_District_Schools$SCHOOL_NUMBER] <- "SIG District School: Yes"
bPlot.data.school.grade$SIG_DISTRICT_SCHOOL[bPlot.data.school.grade$SCHOOL_NUMBER %in% SIG_District_Schools$SCHOOL_NUMBER] <- "SIG District School: Yes"
bPlot.data.instructor$SIG_DISTRICT_SCHOOL[bPlot.data.instructor$SCHOOL_NUMBER %in% SIG_District_Schools$SCHOOL_NUMBER] <- "SIG District School: Yes"
bPlot.data.instructor.grade$SIG_DISTRICT_SCHOOL[bPlot.data.instructor.grade$SCHOOL_NUMBER %in% SIG_District_Schools$SCHOOL_NUMBER] <- "SIG District School: Yes"


### Message

bPlot.message <- c("grid.text(x=unit(50, 'native'), y=unit(50, 'native'), 'CONFIDENTIAL - DO NOT DISTRIBUTE', rot=-30, gp=gpar(col='grey80', cex=2.9, alpha=0.8, fontface=2))",
        "grid.lines(x=unit(50, 'native'), y=c(0,1), gp=gpar(col='grey40', lwd=1.5, lty=2, alpha=0.5))")


### Save files

write.table(bPlot.data.school, file="Data/Mississippi_District_by_School_Summary_Data_2012_2013.dat", na="", row.names=FALSE, quote=FALSE, sep="|")
write.table(bPlot.data.school.grade, file="Data/Mississippi_District_by_School_by_Grade_Summary_Data_2012_2013.dat", na="", row.names=FALSE, quote=FALSE, sep="|")
write.table(bPlot.data.instructor, file="Data/Mississippi_District_by_School_by_Instructor_Summary_Data_2012_2013.dat", na="", row.names=FALSE, quote=FALSE, sep="|")
write.table(bPlot.data.instructor.grade, file="Data/Mississippi_District_by_School_by_Instructor_by_Grade_Summary_Data_2012_2013.dat", na="", row.names=FALSE, quote=FALSE, sep="|")


###
### Subset based upon minimum
###

bPlot.data.school <- subset(bPlot.data.school, !is.na(MEDIAN_SGP) & MEDIAN_SGP_COUNT_2_YEAR >= 10)
bPlot.data.school.grade <- subset(bPlot.data.school.grade, !is.na(MEDIAN_SGP) & MEDIAN_SGP_COUNT_2_YEAR >= 10)
bPlot.data.instructor <- subset(bPlot.data.instructor, !is.na(MEDIAN_SGP) & MEDIAN_SGP_COUNT_2_YEAR >= 10)
bPlot.data.instructor.grade <- subset(bPlot.data.instructor.grade, !is.na(MEDIAN_SGP) & MEDIAN_SGP_COUNT_2_YEAR >= 10)


###
### Construction of bubblePlots
###

#### Bubble Plot 1: Growth by Current Achievement (WHOLE_STATE)

for (i in MS_content_areas) {
tmp.bPlot.data.school <- subset(bPlot.data.school, CONTENT_AREA==i)
test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]


bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=NULL,
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school[["DISTRICT_NAME"]], ": ", tmp.bPlot.data.school[["SCHOOL_NAME"]], sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1="Mississippi School Performance",
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "State_School_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 1a: Growth by Current Achievement (WHOLE_STATE with each DISTRICT HIGHLIGHTED)

for (i in MS_content_areas) {
tmp.bPlot.data.school <- subset(bPlot.data.school, CONTENT_AREA==i)
tmp.bPlot.data.school$DISTRICT_NAME <- factor(tmp.bPlot.data.school$DISTRICT_NAME)
for (j in levels(tmp.bPlot.data.school$DISTRICT_NAME)[table(tmp.bPlot.data.school$DISTRICT_NAME)!=0]) {

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school[["DISTRICT_NAME"]]==j),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=tmp.bPlot.data.school[["SCHOOL_NAME"]],
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste(j, "School Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(j, "2012_2013", capwords(i), "School_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "District", j),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}

#### Bubble Plot 1c: Growth by Current Achievement (WHOLE_STATE by TEACHER)

for (i in MS_content_areas) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=NULL,
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["DISTRICT_NAME"]], ": ", tmp.bPlot.data.instructor[["SCHOOL_NAME"]], " -- ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST_NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1="Mississippi Teacher Performance",
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.09),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "State_Teacher_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


for (i in c("READING_LANGUAGE_ARTS", "MATHEMATICS")) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]],
        bubble_plot_data.SUBSET=NULL,
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2011-2012 Prior Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2011-2012 Prior Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["DISTRICT_NAME"]], ": ", tmp.bPlot.data.instructor[["SCHOOL_NAME"]], " -- ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST_NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1="Mississippi Teacher Performance",
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.09),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "State_Teacher_Performance (Prior Achievement)", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 1d: Growth by Current Achievement (WHOLE_STATE with each DISTRICT HIGHLIGHTED by TEACHER)

for (i in MS_content_areas) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)
tmp.bPlot.data.instructor$DISTRICT_NAME <- factor(tmp.bPlot.data.instructor$DISTRICT_NAME)
for (j in levels(tmp.bPlot.data.instructor$DISTRICT_NAME)[table(tmp.bPlot.data.instructor$DISTRICT_NAME)!=0]) {

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["DISTRICT_NAME"]]==j),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste(j, "Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.09),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(j, "2012_2013", capwords(i), "Teacher_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "District", j),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}

#### Bubble Plot 1d: Growth by Prior Achievement (WHOLE_STATE with each DISTRICT HIGHLIGHTED by TEACHER)

for (i in c("MATHEMATICS", "READING_LANGUAGE_ARTS")) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)
tmp.bPlot.data.instructor$DISTRICT_NAME <- factor(tmp.bPlot.data.instructor$DISTRICT_NAME)
for (j in levels(tmp.bPlot.data.instructor$DISTRICT_NAME)[table(tmp.bPlot.data.instructor$DISTRICT_NAME)!=0]) {

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["DISTRICT_NAME"]]==j),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2011-2012 Prior Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2011-2012 Prior Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste(j, "Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.03, 0.09),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(j, "2012_2013", capwords(i), "Teacher_Performance (Prior Achievement)", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "District", j),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}



#### Bubble Plot 1b: Growth by Current Achievement (WHOLE_STATE with each DISTRICT HIGHLIGHTED and SCHOOLS individually labeled)

#for (i in MS_content_areas) {
#for (j in levels(bPlot.data.school$DISTRICT_NAME)[table(bPlot.data.school$DISTRICT_NAME)!=0]) {
#tmp.bPlot.data.school <- subset(bPlot.data.school, CONTENT_AREA==i)
#for (k in subset(tmp.bPlot.data.school, DISTRICT_NAME==j)[["SCHOOL_NAME"]]) {

#bubblePlot(
#        bubble_plot_data.X=tmp.bPlot.data.school[["MEDIAN_SGP"]],
#        bubble_plot_data.Y=tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]],
#        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school[["DISTRICT_NAME"]]==j),
#        bubble_plot_data.INDICATE=which(tmp.bPlot.data.school[["SCHOOL_NAME"]]==k),
#        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
#        bubble_plot_data.SIZE=tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]],
#        bubble_plot_data.LEVELS=NULL,
#        bubble_plot_data.BUBBLE_TIPS_LINES=list(
#                paste(tmp.bPlot.data.school[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]], ")", sep=""),
#                paste(tmp.bPlot.data.school[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
#                paste(tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
#        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
#        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
#        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
#        bubble_plot_labels.LEVELS=NULL,
#        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
#                "2012-2013 Median SGP (Count)",
#                "2 Year Median SGP (Count)",
#                "2012-2013 Percent at/above Proficient (Count)"),
#        bubble_plot_labels.BUBBLE_TITLES=as.character(tmp.bPlot.data.school[["SCHOOL_NAME"]]),
#        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
#        bubble_plot_titles.SUB1=paste(j, "School Performance"),
#        bubble_plot_titles.SUB2=paste("2012-2013 MCT", capwords(i)),
#        bubble_plot_titles.LEGEND1="School Size",
#        bubble_plot_titles.LEGEND2_P1=NULL,
#        bubble_plot_titles.LEGEND2_P2=NULL,
#
#        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
#        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
#        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
#        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
#        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
#        bubble_plot_configs.BUBBLE_COLOR=NULL,
#        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
#        bubble_plot_configs.BUBBLE_TIPS="TRUE",
#        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
#        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
#        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
#        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
#        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
#        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(k, "2012_2013", capwords(i), "Performance", sep="_"), ".pdf", sep=""),
#        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "District", "School", j),
#        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
#}
#}
#}


#### Bubble Plot 2a: Growth by Current Achievement (PILOT Schools Highlighted)

for (i in c("MATHEMATICS", "READING_LANGUAGE_ARTS")) {
tmp.bPlot.data.school <- subset(bPlot.data.school, CONTENT_AREA==i)

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school[["PILOT_DISTRICT_SCHOOL"]]=="Pilot District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school[["DISTRICT_NAME"]], ": ", tmp.bPlot.data.school[["SCHOOL_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste("Mississippi Pilot School Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Pilot_School Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 2b: Growth by Current Achievement (PILOT Schools Highlighted by Teacher)

for (i in c("MATHEMATICS", "READING_LANGUAGE_ARTS")) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["PILOT_DISTRICT_SCHOOL"]]=="Pilot District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste("Mississippi Pilot School Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Pilot_School Teacher Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 2b: Growth by Prior Achievement (PILOT Schools Highlighted by Teacher PRIOR Achievement)

for (i in c("READING_LANGUAGE_ARTS", "MATHEMATICS")) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["PILOT_DISTRICT_SCHOOL"]]=="Pilot District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2011-2012 Prior Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2011-2012 Prior Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste("Mississippi Pilot School Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Pilot_School Teacher Performance (Prior Achievement)", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 2b: Growth by Prior Achievement (PILOT Schools Highlighted by Teacher)

for (i in c("READING_LANGUAGE_ARTS", "MATHEMATICS")) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["PILOT_DISTRICT_SCHOOL"]]=="Pilot District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2011-2012 Prior Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2011-2012 Prior Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste("Mississippi Pilot School Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Pilot_School Teacher Performance (Prior Achievement)", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}

#### Bubble Plot 2a: Growth by Current Achievement (Pilot School by GRADE)

for (i in c("MATHEMATICS", "READING_LANGUAGE_ARTS")) {
for (j in 4:8) {
tmp.bPlot.data.school.grade <- subset(bPlot.data.school.grade, CONTENT_AREA==i & GRADE==j & !is.na(MEDIAN_SGP))

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school.grade[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school.grade[["PILOT_DISTRICT_SCHOOL"]]=="Pilot District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school.grade[["DISTRICT_NAME"]], ", ", tmp.bPlot.data.school.grade[["SCHOOL_NAME"]], ": Grade ", j, sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste("Mississippi Pilot School, Grade", j, "Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Grade Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Grade", j, "Pilot_School_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}



#### Bubble Plot 2b: Growth by Current Achievement (SIG Schools Highlighted)

for (i in MS_content_areas) {
tmp.bPlot.data.school <- subset(bPlot.data.school, CONTENT_AREA==i)

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school[["SIG_DISTRICT_SCHOOL"]]=="SIG District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school[["DISTRICT_NAME"]], ": ", tmp.bPlot.data.school[["SCHOOL_NAME"]], sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste("Mississippi SIG School Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "SIG_School_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 2b: Growth by Current Achievement (SIG Schools Highlighted by TEACHER)

for (i in MS_content_areas) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["SIG_DISTRICT_SCHOOL"]]=="SIG District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste("Mississippi SIG School Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "SIG_School_Teacher Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}


#### Bubble Plot 2b: Growth by Prior Achievement (SIG Schools Highlighted by TEACHER)

for (i in c("READING_LANGUAGE_ARTS", "MATHEMATICS")) {
tmp.bPlot.data.instructor <- subset(bPlot.data.instructor, CONTENT_AREA==i)

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.instructor[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.instructor[["SIG_DISTRICT_SCHOOL"]]=="SIG District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.instructor[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR"]], " (", tmp.bPlot.data.instructor[["PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2011-2012 Prior Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(10, 25, 50, 100),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2011-2012 Prior Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.instructor[["SCHOOL_NAME"]], ": ", tmp.bPlot.data.instructor[["INSTRUCTOR_FIRST NAME"]], " ", tmp.bPlot.data.instructor[["INSTRUCTOR_LAST_NAME"]], sep=""),
        bubble_plot_titles.MAIN="MCT2 Growth & Achievement",
        bubble_plot_titles.SUB1=paste("Mississippi SIG School Teacher Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013 MCT2", capwords(i)),
        bubble_plot_titles.LEGEND1="Classroom Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "SIG_School_Teacher Performance (Prior Achievement)", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}



#### Bubble Plot 2b: Growth by Current Achievement (SIG School by GRADE)

for (i in MS_content_areas) {
for (j in 4:8) {
tmp.bPlot.data.school.grade <- subset(bPlot.data.school.grade, CONTENT_AREA==i & GRADE==j & !is.na(MEDIAN_SGP))

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school.grade[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school.grade[["SIG_DISTRICT_SCHOOL"]]=="SIG District School: Yes"),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school.grade[["DISTRICT_NAME"]], ", ", tmp.bPlot.data.school.grade[["SCHOOL_NAME"]], ": Grade ", j, sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste("Mississippi SIG School, Grade", j, "Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="Grade Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Grade", j, "SIG_School_Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}


#### Bubble Plot 3: Growth by Current Achievement (WHOLE_STATE by GRADE)

for (i in MS_content_areas) {
for (j in 4:8) {
tmp.bPlot.data.school.grade <- subset(bPlot.data.school.grade, CONTENT_AREA==i & GRADE==j & !is.na(MEDIAN_SGP))

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school.grade[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=NULL,
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school.grade[["DISTRICT_NAME"]], ", ", tmp.bPlot.data.school.grade[["SCHOOL_NAME"]], ": Grade ", j, sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste("Mississippi Grade", j, "Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="Grade Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("MS_2012_2013", capwords(i), "Grade", j, "Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "State"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}



#### Bubble Plot 3a: Growth by Current Achievement (DISTRICT by GRADE)

for (i in MS_content_areas) {
pre.tmp.bPlot.data.school.grade <- subset(bPlot.data.school.grade, CONTENT_AREA==i & !is.na(MEDIAN_SGP))
for (j in unique(pre.tmp.bPlot.data.school.grade$GRADE)) {
tmp.bPlot.data.school.grade <- subset(pre.tmp.bPlot.data.school.grade, GRADE==j)
tmp.bPlot.data.school.grade$DISTRICT_NAME <- factor(tmp.bPlot.data.school.grade$DISTRICT_NAME)
for (k in levels(tmp.bPlot.data.school.grade$DISTRICT_NAME)[table(tmp.bPlot.data.school.grade$DISTRICT_NAME)!=0]) {

test_label <- MS_content_areas_test_type[which(MS_content_areas==i)]

bubblePlot(
        bubble_plot_data.X=tmp.bPlot.data.school.grade[["MEDIAN_SGP"]],
        bubble_plot_data.Y=tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]],
        bubble_plot_data.SUBSET=which(tmp.bPlot.data.school.grade[["DISTRICT_NAME"]]==k),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]],
        bubble_plot_data.LEVELS=NULL,
        bubble_plot_data.BUBBLE_TIPS_LINES=list(
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["MEDIAN_SGP_2_YEAR"]], " (", tmp.bPlot.data.school.grade[["MEDIAN_SGP_COUNT_2_YEAR"]], ")", sep=""),
                paste(tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT"]], " (", tmp.bPlot.data.school.grade[["PERCENT_AT_ABOVE_PROFICIENT_COUNT"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2012-2013 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2012-2013 Percent at/above Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=NULL,
        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
                "2012-2013 Median SGP (Count)",
                "2 Year Median SGP (Count)",
                "2012-2013 Percent at/above Proficient (Count)"),
        bubble_plot_labels.BUBBLE_TITLES=paste(tmp.bPlot.data.school.grade[["DISTRICT_NAME"]], ": ", tmp.bPlot.data.school.grade[["SCHOOL_NAME"]], " Grade ", j, sep=""),
        bubble_plot_titles.MAIN=paste(test_label, "Growth & Achievement"),
        bubble_plot_titles.SUB1=paste("Mississippi Grade", j, "Performance"),
        bubble_plot_titles.SUB2=paste("2012-2013", test_label, capwords(i)),
        bubble_plot_titles.LEGEND1="Grade Size",
        bubble_plot_titles.LEGEND2_P1=NULL,
        bubble_plot_titles.LEGEND2_P2=NULL,

        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 1, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(k, "2012_2013", capwords(i), "Grade", j, "Performance", sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "District", k),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
}
}
}


### INSTRUCTOR LEVEL PLOTS

load("Data/Mississippi_SGP.Rdata")

visualizeSGP(Mississippi_SGP,
	plot.types="bubblePlot",
	bPlot.content_areas=c("MATHEMATICS", "READING_LANGUAGE_ARTS"),
	bPlot.draft=TRUE,
	bPlot.styles=153)

