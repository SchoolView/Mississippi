####################################################################
###
### SGP analysis of Mississippi Data
###
####################################################################

### Load SGP package

require(SGP)
options(error=recover)


### Load data

load("Data/Mississippi_Data_LONG_2012_2013.Rdata")
load("Data/Mississippi_SGP.Rdata")
load("Data/Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2012_2013.Rdata")


### prepareSGP via updateSGP

Mississippi_SGP <- updateSGP(Mississippi_SGP, Mississippi_Data_LONG_2012_2013, steps="prepareSGP")
Mississippi_SGP@Data_Supplementary <- data.table(rbind.fill(Mississippi_SGP@Data_Supplementary$INSTRUCTOR_NUMBER, Mississippi_Data_LONG_INSTRUCTOR_NUMBER_2012_2013), key=c("ID", "CONTENT_AREA", "YEAR"))


### analyzeSGP for MATHEMATICS & READING_LANGUAGE_ARTS

Mississippi_SGP <- analyzeSGP(Mississippi_SGP,
			years="2012_2013",
			content_areas=c("MATHEMATICS", "READING_LANGUAGE_ARTS"),
			sgp.percentiles=TRUE,
			sgp.projections=TRUE,
			sgp.projections.lagged=TRUE,
			sgp.percentiles.baseline=TRUE,
			sgp.projections.baseline=TRUE,
			sgp.projections.lagged.baseline=TRUE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=20, BASELINE_PERCENTILES=20, PROJECTIONS=12, LAGGED_PROJECTIONS=12)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### analyzeSGP for SATP Exams (ALGEBRA, BIOLOGY, ENGLISH, HISTORY)

source("SGP_CONFIG/EOCT/2012_2013/ALGEBRA.R")
source("SGP_CONFIG/EOCT/2012_2013/BIOLOGY.R")
source("SGP_CONFIG/EOCT/2012_2013/ENGLISH.R")
source("SGP_CONFIG/EOCT/2012_2013/SCIENCE.R")

MS_EOCT_2012_2013.config <- c(
		ALGEBRA.2012_2013.config,
		BIOLOGY.2012_2013.config,
		ENGLISH.2012_2013.config,
		SCIENCE.2012_2013.config)

MS_EOCT.config <- MS_EOCT_2012_2013.config

Mississippi_SGP <- analyzeSGP(
			Mississippi_SGP,
			sgp.percentiles=TRUE,
			sgp.projections=FALSE,
			sgp.projections.lagged=FALSE,
			sgp.percentiles.baseline=FALSE,
			sgp.projections.baseline=FALSE,
			sgp.projections.lagged.baseline=FALSE,
			sgp.config=MS_EOCT.config,
                        parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=30)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### combineSGP, summarizeSGP, visualizeSGP, outputSGP

Mississippi_SGP <- abcSGP(Mississippi_SGP,
			years="2012_2013",
			steps=c("combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
			sgPlot.demo.report=TRUE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SUMMARY=30, GA_PLOTS=20, SG_PLOTS=1)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")
