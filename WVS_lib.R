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
# create full map of variable name from each wave to the integrated one
################
map.Long.to.Wave <- function() {
  require(readxl)
  
  integratedDictionary <- "WVS_Values Surveys Integrated Dictionary_TimeSeries_v_2014-04-25.xls"
  iDict <- read_excel(file.path(datapath, integratedDictionary),
                      skip=3)
  # rename the fields to be meaningful
  WVfield <- 4:9  # based on current field format
  names(iDict)[WVfield] <- paste0("WV", 1:length(WVfield))
  # take first string before space in each as the variable name
  for (i in WVfield) {
    iDict[, i] <- sapply(strsplit(iDict[, i], " "), function(x) x[1])
  }
  
  return(iDict)
}

################
# create full map of variable name from each wave to the integrated one
################
map.Wave <- function(selectwave) {
  # get full mapping table
  waveTable <- map.Long.to.Wave()
  
  #identify column for appropriate wave
  waveCol <- grep(paste0("WV", selectwave), names(waveTable))

  # remove rows with empty value for this wave
  waveTable <- waveTable[waveTable[, waveCol] !="", ]
    
  # remove other waves
  waveRemove <- grep("WV", names(waveTable))
  waveTable <- waveTable[, -waveRemove[waveRemove != waveCol]]
  
  return(waveTable)
}

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
  exc.phrases <- c("(*) See annexe", "CS Codes")
  if (valuerange %in% exc.phrases) is.cat=TRUE
  
  return(is.cat)
}

################
# load longitudinal data for WVS filtered for Finland and Singapore
# set up for Happiness as dependent variable
################
load.WVS <- function(sourcefile, codebook, fieldinfo, mainvar,
                     happinessfield, countryfield, 
                     nafield="") {
  # na fields are fields with NA value, default none
  # negfield above are for fields with meaningful value range in the negative
  # e.g. is -2 to 2 where -2 is strongly disagree and 2 is strongly agree
  # excluding fields where negative is used to indicate absence of data
  
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
  
  #special treatment for fields which are known to have NAs:
  # categorise to -5 (i.e. unknown) before converting to factor
  # TODO: automate discovery of NAs rather than specified
  naloop <- which(names(Bin) %in% nafield)
  for (i in naloop) {
    Bin[is.na(Bin[, i]), i] <- -5  
  }

  #change to factor those which are factorsWV6_Codebook_v_2014_11_07WV6_Codebook_v_2014_11_07
  is.cat <- sapply(fieldsRange$VALUE_RANGE, is.categorical, USE.NAMES=FALSE)
  idx <- which(names(Bin) %in% fieldsRange$VARIABLE[is.cat])
  for (i in idx) Bin[, i] <- as.factor(Bin[, i])

  # rename for convenience
  names(Bin)[names(Bin)==happinessfield] <- "Happiness"
  names(Bin)[names(Bin)==countryfield] <- "Country"
  
  # set readable level for key fields
  levels(Bin$Country) <- c("Finland", "Singapore")
  happinessSurveyLegend <- as.character(c(-5:-1, 1:4))
  happinessSurveyLabels <- c("Inappropriate response", "Not asked in survey", 
                             "Not applicable", "No answer", "Don't know", "Very happy",
                             "Quite happy", "Not very happy", "Not at all happy")
  Bin$Happiness <- factor(Bin$Happiness, levels=happinessSurveyLegend,
                          labels=happinessSurveyLabels)

  # filter out those without data on Happiness
  # Bin <- Bin[!(Bin$Happiness %in% c("No answer", "Don't know")), ]  # eliminate those with unknown/missing answer etc on happiness
  Bin <- Bin[as.numeric(Bin$Happiness) > 0, ]  # eliminate those with unknown/missing answer etc on happiness
  #refactor to eliminate non-existent label
  Bin$Happiness <- factor(Bin$Happiness)
  
  # check how many different values are in the field
  # dropping fields which only have one value (useless for prediction and throws error)
  responses <- apply(Bin, 2, function(x) length(table(x)))
  Bin <- Bin[ , -which(responses==1)]

  return(Bin)
}


############
# convenience function to load longitudinal happiness data
###########
load.WVS.long.happy <- function() {
  load.WVS(sourcefile="WV_long.RData",
           codebook="filtered WVS_Values Surveys Integrated Dictionary_TimeSeries_v_2014-04-25.csv",
           fieldinfo="WVS_L_filtered_valuerange.csv",
           mainvar="WVL",
           happinessfield="A008",
           countryfield="S003",
           nafield=c("Y003"))
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

################
# merge non-answers (e.g. negative values indicating Not Applicable, Don't Know, etc)
# into one level
################
merge.nonanswers <- function(wvsdata) {
  # hard coded exception at the moment to exclude:
  # Y003 the only one where negative is not a non-answer
  # Country and Happiness, which has text levels
  exclude <- c("Happiness", "Country", "Y003")
  
  ## also non-factors
  nonfactors <- which(sapply(wvsdata, class)!="factor")
  exclude <- c(exclude, names(wvsdata)[nonfactors])
  
  #note that even numerics (e.g. age) have some illogical negative number
  #handling of that left outside this merging
  idx <- which(!(names(wvsdata) %in% exclude))
  
  temp <- wvsdata[, idx]
  #convert back to numeric first
  temp <- lapply(temp, function(x) as.numeric(as.character(x)))
  #standardise all non-positive to 0
  temp <- lapply(temp, function(x) { x[x < 1] <- 0; x})
  #convert back to factor
  temp <- lapply(temp, factor)
  
  wvsdata[, idx] <- temp
  
  return(wvsdata)
}
