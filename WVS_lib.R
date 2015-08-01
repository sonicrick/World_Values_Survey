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
tuneLength <- 5
CVfolds <- 5
CVreps <- 5

# create manual seeds vector for parallel processing repeatibility
seedNum <- CVfolds * CVreps + 1
seedLen <- (CVfolds + tuneLength)  #supposed to be the number of models evaluated: maybe number of folds + tunelength?
set.seed(321)
seeds <- vector(mode = "list", length = seedNum)
for(i in 1:(seedNum-1)) seeds[[i]] <- sample.int(1000, seedLen)  
## For the last model:
seeds[[seedNum]] <- sample.int(1000, 1)

#basic control
fitControl <- trainControl(
  method = "repeatedcv",
  number = CVfolds,
  repeats = CVreps,
  seeds=seeds
  )

# train control for Binary classifier, necessary when optimizing sensitivity and specificity
fitTwoClass <- trainControl(
  method = "repeatedcv",
  number = CVfolds,
  repeats = CVreps,
  classProbs=TRUE,
  summaryFunction = twoClassSummary,
  seeds = seeds
)

# convenience function to create new fitTwoClass given different seeds; to try find a seed that doesn't cause error
createFTC <- function (startseed) {
  seedNum <- CVfolds * CVreps + 1
  seedLen <- (CVfolds + tuneLength)  #supposed to be the number of models evaluated: maybe number of folds + tunelength?
  set.seed(startseed)
  seeds <- vector(mode = "list", length = seedNum)
  for(i in 1:(seedNum-1)) seeds[[i]] <- sample.int(1000, seedLen)  
  ## For the last model:
  seeds[[seedNum]] <- sample.int(1000, 1)
  FTC <- trainControl(
    method = "repeatedcv",
    number = CVfolds,
    repeats = CVreps,
    classProbs=TRUE,
    summaryFunction = twoClassSummary,
    seeds = seeds
  )
  return(FTC)
}

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
# load data for WVS filtered for Finland and Singapore (longitudinal or wave)
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
  meaningful <- c("Very happy", "Quite happy", "Not very happy", "Not at all happy")
  Bin$Happiness <- factor(Bin$Happiness, levels=happinessSurveyLegend,
                          labels=happinessSurveyLabels)

  # filter out those without data on Happiness
  # Bin <- Bin[as.numeric(Bin$Happiness) > 0, ]  # eliminate those with unknown/missing answer etc on happiness
  Bin <- Bin[Bin$Happiness %in% meaningful, ]  # eliminate those with unknown/missing answer etc on happiness
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


################
# calculate accuracy against a test data set for a fit model from caret
# for a particular targetfueld
# when changing targetfield, remember to feed testfile with same field as what is trained for fitmodel
# (is usually omitting other target fields, e.g when targetfield is "Unhappiness", testfile may have [, <exclude "Happiness"])
################

accuracy_chk <- function(fitmodel, testfile, targetfield) {
  pred <- predict(fitmodel, testfile)
  chkdf <- data.frame(predicted=pred, actual=testfile[[targetfield]])
  crosstab <- with(chkdf, table(predicted, actual))
  chksum <- chkdf %>% group_by(predicted, actual) %>% summarize(count=n())
  accur <- with(chksum, sum(count[predicted==actual]))/nrow(testfile)
  return(list(chkdf=chkdf,
              crosstab=crosstab,
              chksum=chksum,
              accur=accur))
}


################
# convenience function for training unhappiness
# default to training by accuracy and numeric data only
################

trainUnhappiness <- function(tmethod, tdata=dnumTrain, control=fitControl, tmetric="Accuracy", display=tmethod) {
  cat(paste0(display, "\n"))
  set.seed(12345) # need to set same seed for all training to have same fold separation?
  ptm <- proc.time()
  fitModel <- train(
    Unhappiness ~ ., data = tdata,
    method = tmethod,
    trControl = control,
    tuneLength = tuneLength,
    metric=tmetric
  )
  time1 <- proc.time()-ptm
  cat(time1)
  return(fitModel)
}