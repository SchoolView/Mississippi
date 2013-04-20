####################################################################
###
### SGP analysis of Mississippi Data
###
####################################################################

### Load SGP package

require(SGP)
options(error=recover)


### Load data

#load("Data/Mississippi_Data_LONG.Rdata")
load("Data/Mississippi_SGP.Rdata")


### prepareSGP

Mississippi_SGP <- prepareSGP(Mississippi_SGP)


### analyzeSGP for MATHEMATICS & READING_LANGUAGE_ARTS

Mississippi_SGP <- analyzeSGP(Mississippi_SGP,
			content_areas=c("MATHEMATICS", "READING_LANGUAGE_ARTS"),
			sgp.percentiles=TRUE,
			sgp.projections=TRUE,
			sgp.projections.lagged=TRUE,
			sgp.percentiles.baseline=TRUE,
			sgp.projections.baseline=TRUE,
			sgp.projections.lagged.baseline=TRUE,
			sgp.use.my.coefficient.matrices=TRUE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=20, BASELINE_PERCENTILES=20, PROJECTIONS=12, LAGGED_PROJECTIONS=12)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### analyzeSGP for SATP Exams (ALGEBRA, BIOLOGY, ENGLISH, HISTORY)

source("SGP_CONFIG/EOCT/2009_2010/ALGEBRA.R")
source("SGP_CONFIG/EOCT/2009_2010/BIOLOGY.R")
source("SGP_CONFIG/EOCT/2009_2010/ENGLISH.R")
source("SGP_CONFIG/EOCT/2009_2010/SCIENCE.R")
source("SGP_CONFIG/EOCT/2010_2011/ALGEBRA.R")
source("SGP_CONFIG/EOCT/2010_2011/BIOLOGY.R")
source("SGP_CONFIG/EOCT/2010_2011/ENGLISH.R")
source("SGP_CONFIG/EOCT/2010_2011/HISTORY.R") ### NOTE HISTORY goes back three years and is currently only available for 2010_2011 analyses
source("SGP_CONFIG/EOCT/2010_2011/SCIENCE.R")
source("SGP_CONFIG/EOCT/2011_2012/ALGEBRA.R")
source("SGP_CONFIG/EOCT/2011_2012/BIOLOGY.R")
source("SGP_CONFIG/EOCT/2011_2012/ENGLISH.R")
source("SGP_CONFIG/EOCT/2011_2012/SCIENCE.R")

MS_EOCT_2009_2010.config <- c(
		ALGEBRA.2009_2010.config,
		BIOLOGY.2009_2010.config,
		ENGLISH.2009_2010.config,
		SCIENCE.2009_2010.config)

MS_EOCT_2010_2011.config <- c(
		ALGEBRA.2010_2011.config,
		BIOLOGY.2010_2011.config,
		ENGLISH.2010_2011.config,
		HISTORY.2010_2011.config,
		SCIENCE.2010_2011.config)

MS_EOCT_2011_2012.config <- c(
		ALGEBRA.2011_2012.config,
		BIOLOGY.2011_2012.config,
		ENGLISH.2011_2012.config,
		SCIENCE.2011_2012.config)

MS_EOCT.config <- c(MS_EOCT_2009_2010.config, MS_EOCT_2010_2011.config, MS_EOCT_2011_2012.config)

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
			steps=c("combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
			sgPlot.demo.report=TRUE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SUMMARY=30, GA_PLOTS=20, SG_PLOTS=1)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")
