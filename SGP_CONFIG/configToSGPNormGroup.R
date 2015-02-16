###################################################################################################
###
### Script to convert SGP configurations for EOCT analyses to SGP_NORM_GROUP preference tables
###
###################################################################################################

### Load packages

require("data.table")
options(error=recover)

### utility function

configToSGPNormGroup <- function(sgp.config) {
        tmp.norm.group <- tmp.norm.group.baseline <- paste(sgp.config$sgp.panel.years, paste(sgp.config$sgp.content.areas, unlist(sgp.config$sgp.grade.sequences), sep="_"), sep="/")
	if ("sgp.norm.group.preference" %in% names(sgp.config)) {
		return(
			data.table(
				SGP_NORM_GROUP=paste(tmp.norm.group, collapse="; "), 
				SGP_NORM_GROUP_BASELINE=paste(tmp.norm.group.baseline, collapse="; "),
				PREFERENCE=as.integer(sgp.config$sgp.norm.group.preference)
			)
		)
	} else {
		return(NULL)
	}
}


### Load and create 2009_2010, 2010_2011, and 2011_2012 EOCT Configuration

source("EOCT/2009_2010/ALGEBRA.R")
source("EOCT/2009_2010/BIOLOGY.R")
source("EOCT/2009_2010/ENGLISH.R")
source("EOCT/2009_2010/SCIENCE.R")
source("EOCT/2010_2011/ALGEBRA.R")
source("EOCT/2010_2011/BIOLOGY.R")
source("EOCT/2010_2011/ENGLISH.R")
source("EOCT/2010_2011/HISTORY.R") ### NOTE HISTORY goes back three years and is currently only available for 2010_2011 analyses
source("EOCT/2010_2011/SCIENCE.R")
source("EOCT/2011_2012/ALGEBRA.R")
source("EOCT/2011_2012/BIOLOGY.R")
source("EOCT/2011_2012/ENGLISH.R")
source("EOCT/2011_2012/SCIENCE.R")
source("EOCT/2012_2013/ALGEBRA.R")
source("EOCT/2012_2013/BIOLOGY.R")
source("EOCT/2012_2013/ENGLISH.R")
source("EOCT/2012_2013/SCIENCE.R")
source("EOCT/2013_2014/ALGEBRA.R")
source("EOCT/2013_2014/BIOLOGY.R")
source("EOCT/2013_2014/ENGLISH.R")
source("EOCT/2013_2014/SCIENCE.R")

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

MS_EOCT_2012_2013.config <- c(
		ALGEBRA.2012_2013.config,
                BIOLOGY.2012_2013.config,
                ENGLISH.2012_2013.config,
		SCIENCE.2012_2013.config)

MS_EOCT_2013_2014.config <- c(
		ALGEBRA.2013_2014.config,
                BIOLOGY.2013_2014.config,
                ENGLISH.2013_2014.config,
		SCIENCE.2013_2014.config)


### Create configToNormGroup data.frame

tmp.configToNormGroup <- lapply(MS_EOCT_2009_2010.config, configToSGPNormGroup)

MS_SGP_Norm_Group_Preference_2009_2010 <- data.table(
					YEAR="2009_2010",
					rbindlist(tmp.configToNormGroup))


tmp.configToNormGroup <- lapply(MS_EOCT_2010_2011.config, configToSGPNormGroup)

MS_SGP_Norm_Group_Preference_2010_2011 <- data.table(
					YEAR="2010_2011",
					rbindlist(tmp.configToNormGroup))


tmp.configToNormGroup <- lapply(MS_EOCT_2011_2012.config, configToSGPNormGroup)

MS_SGP_Norm_Group_Preference_2011_2012 <- data.table(
					YEAR="2011_2012",
					rbindlist(tmp.configToNormGroup))

tmp.configToNormGroup <- lapply(MS_EOCT_2012_2013.config, configToSGPNormGroup)

MS_SGP_Norm_Group_Preference_2012_2013 <- data.table(
					YEAR="2012_2013",
					rbindlist(tmp.configToNormGroup))

tmp.configToNormGroup <- lapply(MS_EOCT_2013_2014.config, configToSGPNormGroup)

MS_SGP_Norm_Group_Preference_2013_2014 <- data.table(
					YEAR="2013_2014",
					rbindlist(tmp.configToNormGroup))

MS_SGP_Norm_Group_Preference <- rbind(
			MS_SGP_Norm_Group_Preference_2009_2010,
			MS_SGP_Norm_Group_Preference_2010_2011,
			MS_SGP_Norm_Group_Preference_2011_2012,
			MS_SGP_Norm_Group_Preference_2012_2013,
			MS_SGP_Norm_Group_Preference_2013_2014
			)

MS_SGP_Norm_Group_Preference$SGP_NORM_GROUP <- as.factor(MS_SGP_Norm_Group_Preference$SGP_NORM_GROUP)
MS_SGP_Norm_Group_Preference$SGP_NORM_GROUP_BASELINE <- as.factor(MS_SGP_Norm_Group_Preference$SGP_NORM_GROUP_BASELINE)


### Save result

setkey(MS_SGP_Norm_Group_Preference, YEAR, SGP_NORM_GROUP)
save(MS_SGP_Norm_Group_Preference, file="MS_SGP_Norm_Group_Preference.Rdata")
