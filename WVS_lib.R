#################
# library of common functions for WVS project
#################

setwd("~/GitHub/World_Values_Survey")
datapath <- "~/GitHub/World_Values_Survey/data"

require(caret)

################
# set standard caret training setting across all experiment
################
# train setting
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5)

################
# identify option numbers in the value range of a field
################
extract.field.numbers <- function(valuerange) {
  numsvr <- unlist(strsplit(valuerange, "[\\#\\:\n]"))
  nonnum <- which(is.na(as.numeric(numsvr)))  # stripaway non number
  return(as.numeric(numsvr[-nonnum]))
}

################
# determine if a WVS field is categorical
################
is.categorical <- function(valuerange) {
  # if any value range explanation is above 0, it's categorical (e.g. 1 = Dissatisfied, 10 = Satisfied, etc)
  # non-categorical number may have subzero e.g. -1 for Unknown
  is.cat <- max(extract.field.numbers(valuerange)) > 0
  
  #exception: some has no numbers and say "
  exc.phrases <- c("(*) See annexe")
  if (valuerange %in% exc.phrases) is.cat=TRUE
  
  return(is.cat)
}

################
# load longitudinal data for WVS filtered for Finland and Singapore
# set up for Happiness as dependent variable
################
load.WVS <- function(sourcefile, codebook, fieldinfo, mainvar) {
  load(file.path(datapath, sourcefile))
  fields <- read.csv(file.path(datapath, codebook),
                     stringsAsFactors=FALSE)
  
  #assign dataframe to be evaluated, which may differ in different sourcefile
  WVL <- get(mainvar)
  
  # eliminate blank fields
  varnames <- fields$VARIABLE
  varnames <- varnames[varnames!=""]
  map <- which(names(WVL) %in% varnames)
  Bin <- WVL[, map] # master
  
  # get field possible value and fields to ignore
  fieldsRange <- read.csv(file.path(datapath, fieldinfo),
                          stringsAsFactors = FALSE)
  
  # drop fields to ignore
  Bin <- Bin[, !(names(Bin) %in% fieldsRange$VARIABLE[fieldsRange$IGNORE=="Y"])]
  fieldsRange <- fieldsRange[fieldsRange$IGNORE!="Y", ]  #shorten list of names after eliminating IGNORED
  
  #special treatment for Y003 which has NAs: categorise to -5 (i.e. unknown) before converting to factor
  Bin$Y003[is.na(Bin$Y003)] <- -5
  
  #change to factor those which are factorsWV6_Codebook_v_2014_11_07WV6_Codebook_v_2014_11_07
  is.cat <- sapply(fieldsRange$VALUE_RANGE, is.categorical, USE.NAMES=FALSE)
  idx <- which(names(Bin) %in% fieldsRange$VARIABLE[is.cat])
  for (i in idx) Bin[, i] <- as.factor(Bin[, i])
  
  # set readable level for key fields
  levels(Bin$S003) <- c("Finland", "Singapore")
  levels(Bin$A008) <- c("No answer", "Don't know", "Very happy", "Quite happy", "Not very happy", "Not at all happy")
  
  # filter out those without data on Happiness
  Bin <- Bin[!(Bin$A008 %in% c("No answer", "Don't know")), ]  # eliminate those with unknown/missing answer etc on happiness
  #refactor to eliminate non-existent label
  Bin$A008 <- factor(Bin$A008)
  
  # check how many different values are in the field
  # dropping fields which only have one value (useless for prediction and throws error)
  responses <- apply(Bin, 2, function(x) length(table(x)))
  Bin <- Bin[ , -which(responses==1)]
  
  # rename for convenience
  names(Bin)[names(Bin)=="A008"] <- "Happiness"
  names(Bin)[names(Bin)=="S003"] <- "Country"
  
  return(Bin)
}

############
# convenience function to load longitudinal happiness data
###########
load.WVS.long.happy <- function() {
  load.WVS(sourcefile="WV_long.RData",
           codebook="filtered WVS_Values Surveys Integrated Dictionary_TimeSeries_v_2014-04-25.csv",
           fieldinfo="WVS_L_filtered_valuerange.csv",
           mainvar="WVL")
}

################
# separate data by country
################
separate.country <- function(wvsdata, country) {
  sepdata <- wvsdata[wvsdata$Country==country, ]
  country.fields <- c("Country", "S003A", "S024", "S025")
  sepdata <- sepdata[, !(names(sepdata) %in% country.fields)]
  return(sepdata)
}
