####################################################################
###
### SGP analysis of Mississippi Data 2012
###
####################################################################

### Load SGP package

require(SGP)
options(error=recover)


### Load data

load("Data/Base_Files/Mississippi_Data_LONG_2011_2012.Rdata")
load("Data/Base_Files/Mississippi_SGP.Rdata")


### Merge in 2011_2012 data

Mississippi_SGP@Data <- as.data.table(rbind.fill(Mississippi_SGP@Data, Mississippi_Data_LONG_2011_2012))


### prepareSGP

Mississippi_SGP <- prepareSGP(Mississippi_SGP)

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### Calculate SGPs for MCT first

Mississippi_SGP <- analyzeSGP(Mississippi_SGP,
			content_areas=c("MATHEMATICS", "READING_LANGUAGE_ARTS"),
			years="2011_2012",
			sgp.percentiles=TRUE,
			sgp.projections=TRUE,
			sgp.projections.lagged=TRUE,
			sgp.percentiles.baseline=FALSE,
			sgp.projections.baseline=FALSE,
			sgp.projections.lagged.baseline=FALSE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=5, BASELINE_PERCENTILES=5, PROJECTIONS=5, LAGGED_PROJECTIONS=5)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### Calculate SGPs for SATP (EOC exams)

MS.config <- list(
	ALGEBRA.2011_2012 = list(
                    sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010", "2010_2011", "2011_2012"),
                    sgp.grade.sequences=list(5:9)),
	ALGEBRA.2011_2012 = list(
                    sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010", "2010_2011", "2011_2012"),
                    sgp.grade.sequences=list(5:9)),
	BIOLOGY.2011_2012 = list(
                    sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'SCIENCE', 'BIOLOGY'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010", "2010_2011", "2011_2012"),
                    sgp.grade.sequences=list(5:9)),
	ENGLISH.2011_2012 = list(
                    sgp.content.areas=c('READING_LANGUAGE_ARTS', 'READING_LANGUAGE_ARTS', 'READING_LANGUAGE_ARTS', 'ENGLISH'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010", "2011_2012"),
                    sgp.grade.sequences=list(c(6,7,8,10))))

Mississippi_SGP <- analyzeSGP(
			Mississippi_SGP,
			sgp.percentiles=TRUE,
			sgp.projections=FALSE,
			sgp.projections.lagged=FALSE,
			sgp.percentiles.baseline=FALSE,
			sgp.projections.baseline=FALSE,
			sgp.projections.lagged.baseline=FALSE,
			sgp.config=MS.config,
                        parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=3)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### combineSGP, summarizeSGP, visualizeSGP, outputSGP

Mississippi_SGP <- combineSGP(Mississippi_SGP, years="2011_2012")

Mississippi_SGP <- abcSGP(Mississippi_SGP,
			steps=c("summarizeSGP", "visualizeSGP", "outputSGP"),
			sgPlot.demo.report=TRUE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SUMMARY=30, GA_PLOTS=20, SG_PLOTS=1)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")
