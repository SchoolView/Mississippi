####################################################################
###
### SGP analysis of Mississippi Data
###
####################################################################

### Load SGP package

require(SGP)
options(error=recover)


### Load data

load("Data/Mississippi_Data_LONG.Rdata")
##load("Data/Mississippi_SGP.Rdata")


### Calculate SGPs for MCT first

Mississippi_SGP <- abcSGP(subset(Mississippi_Data_LONG, TEST_ADMINISTRATION=="MCT"),
			state="MS",
			steps=c("prepareSGP", "analyzeSGP"),
			sgp.percentiles=TRUE,
			sgp.projections=TRUE,
			sgp.projections.lagged=TRUE,
			sgp.percentiles.baseline=FALSE,
			sgp.projections.baseline=FALSE,
			sgp.projections.lagged.baseline=FALSE,
			parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=30, BASELINE_PERCENTILES=30, PROJECTIONS=14, LAGGED_PROJECTIONS=14)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### Calculate SGPs for SATP (EOC exams)

Mississippi_SGP@Data <- as.data.table(Mississippi_Data_LONG)
setkey(Mississippi_SGP@Data, VALID_CASE, CONTENT_AREA, YEAR, ID)

MS.config <- list(
	ALGEBRA.2010_2011 = list(
                    sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'MATHEMATICS', 'ALGEBRA'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010", "2010_2011"),
                    sgp.grade.sequences=list(5:8, 6:9)),
	ALGEBRA.2009_2010 = list(
                    sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'ALGEBRA'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010"),
                    sgp.grade.sequences=list(6:8, 7:9)),
	BIOLOGY.2010_2011 = list(
                    sgp.content.areas=c('MATHEMATICS', 'MATHEMATICS', 'SCIENCE', 'BIOLOGY'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010", "2010_2011"),
                    sgp.grade.sequences=list(6:9)),
	BIOLOGY.2009_2010 = list(
                    sgp.content.areas=c('MATHEMATICS', 'SCIENCE', 'BIOLOGY'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2009_2010"),
                    sgp.grade.sequences=list(7:9)),
	ENGLISH.2010_2011 = list(
                    sgp.content.areas=c('READING_LANGUAGE_ARTS', 'READING_LANGUAGE_ARTS', 'ENGLISH'),
                    sgp.panel.years=c("2007_2008", "2008_2009", "2010_2011"),
                    sgp.grade.sequences=list(c(7,8,10))),
	ENGLISH.2009_2010 = list(
                    sgp.content.areas=c('READING_LANGUAGE_ARTS', 'ENGLISH'),
                    sgp.panel.years=c("2007_2008", "2009_2010"),
                    sgp.grade.sequences=list(c(8,10))),
	HISTORY.2010_2011 = list(
                    sgp.content.areas=c('READING_LANGUAGE_ARTS', 'HISTORY'),
                    sgp.panel.years=c("2007_2008", "2010_2011"),
                    sgp.grade.sequences=list(c(8,11))))

Mississippi_SGP <- analyzeSGP(
			Mississippi_SGP,
			sgp.percentiles=TRUE,
			sgp.projections=FALSE,
			sgp.projections.lagged=FALSE,
			sgp.percentiles.baseline=FALSE,
			sgp.projections.baseline=FALSE,
			sgp.projections.lagged.baseline=FALSE,
			sgp.config=MS.config,
                        parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=30)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")


### combineSGP, summarizeSGP, visualizeSGP, outputSGP

Mississippi_SGP <- abcSGP(Mississippi_SGP,
			steps=c("combineSGP", "summarizeSGP", "visualizeSGP", "outputSGP"),
			sgPlot.demo.report=TRUE,
                        parallel.config=list(BACKEND="PARALLEL", WORKERS=list(SUMMARY=30, GA_PLOTS=20, SG_PLOTS=1)))

save(Mississippi_SGP, file="Data/Mississippi_SGP.Rdata")
