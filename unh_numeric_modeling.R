################
# analyzing unhappiness of wave 6 (ie Singapore) data using upsampling
# using numeric-scale predictors for comparison
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')
require(plyr); require(dplyr)

#### NOTE: data processed will be using dnon (i.e. where NA answers are merged -> se load_WV6_for_caret.R)

# manual tweaking of variables to streamline partitioning
# drop "Year of birth" in favour of age
dnon <- dnon[, names(dnon) != "V241"]


# set Unhappiness using dnon 
dnon$Unhappiness <- "Y"
dnon[which(d$Happiness %in% c("Very happy", "Quite happy")), ]$Unhappiness <- "N"
dnon$Unhappiness <- relevel(factor(dnon$Unhappiness), ref="Y")  # caret needs this as factor to train, forced with Y as reference
dnon$Happiness <- NULL  # eliminate the field to avoid confusion
wt <- table(dnon$Unhappiness)/nrow(dnon)  # parameter for class weightage in training below

# read field descriptions in codebook
fieldsRange <- read.csv(file.path(datapath, "WVS_6_valuerange.csv"),
                   stringsAsFactors=FALSE)
#keep only fields in dataset
fieldsRange <- fieldsRange[fieldsRange$VARIABLE %in% names(dnon), ]
numScale <- fieldsRange$VARIABLE[fieldsRange$NonStandard!="Y"]

# split into numeric and non-numeric
dnum <- as.data.frame(sapply(dnon[, (names(dnon) %in% numScale)], as.numeric))
dcat <- dnon[, !((names(dnon) %in% numScale))]

# do preprocessing for numerical
preProcNum <- preProcess(dnum, method=c("range"))
dnum <- predict(preProcNum, dnum)
dnum <- cbind(dnum, Unhappiness=dnon$Unhappiness)

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(dnon$Unhappiness, p=.8, list=FALSE)
dnontrain <- dnon[trainIndex, ]
dnontrain <- upSample(dnontrain, dnontrain$Unhappiness)
dnontest <- dnon[-trainIndex, ]

dnumTrain <- dnum[trainIndex, ]
dnumTrain <- upSample(dnumTrain, dnumTrain$Unhappiness, list=T)[[1]]
dnumTest <- dnum[-trainIndex, ]


##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

models <- c(
  # tried and eliminated because errors
  # "lm", "gam", "knn", "rocc",
  # tried and eliminated because performance not better than C5.0,
  # and results cannot be saved in RData (it links to environment)
  # "J48", "LMT",
  "ctree", "rpart", "ada", "C5.0", "lda",  "nb", "svmLinear"
  )

# train 1: optimize by Accuracy (default)
listUnhAcc <- lapply(models, trainUnhappiness)

# train 2: by ROC
require(pROC)
listTwoClass <- lapply(models, function(x) trainUnhappiness(tmethod=x, control=fitTwoClass, tmetric="ROC" ))
# tst <- trainUnhappiness(tmethod="rocc", control=fitTwoClass, tmetric="ROC")

# train 3: by sensitivity
# NOTE: from past runs, observed that only a few methods have different outcome optimizing for sensitivity vs for ROC
listTwoClassSens <- lapply(models, function(x) trainUnhappiness(tmethod=x, control=fitTwoClass, tmetric="Sens" ))


#consolidating and saving
names(listUnhAcc) <- models
resampsNumUnh <- resamples(listUnhAcc)

names(listTwoClass) <- models
resampsNumROCUnh <- resamples(listTwoClass)

names(listTwoClassSens) <- models
resampsNumSensUnh <- resamples(listTwoClassSens)

accList <- lapply(listUnhAcc, accuracy_chk, dnumTest, "Unhappiness")
allAccur <- sapply(accList, function(x) x$accur)
unhAccur <- sapply(accList, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhRecall <- sapply(accList, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

ROCList <- lapply(listTwoClass, accuracy_chk, dnumTest, "Unhappiness")
allROCAccur <- sapply(ROCList, function(x) x$accur)
unhROCAccur <- sapply(ROCList, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhROCRecall <- sapply(ROCList, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

SensList <- lapply(listTwoClassSens, accuracy_chk, dnumTest, "Unhappiness")
allSensAccur <- sapply(SensList, function(x) x$accur)
unhSensAccur <- sapply(SensList, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhSensRecall <- sapply(SensList, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

shortlist_model <- c("lda", "rpart", "ada", "C5.0", "nb")
shortlist <- lapply(shortlist_model, function(x) ROCList[[x]]$crosstab)
shortlist_sens <- lapply(shortlist_model, function(x) SensList[[x]]$crosstab)
names(shortlist) <- shortlist_model
names(shortlist_sens) <- shortlist_model
# shortlist <- ROCList[[shortlist_model]]$crosstab

save(resampsNumUnh,
     listUnhAcc,
     file=file.path(datapath, "numeric_unhappiness.Rdata")
     )

save(resampsNumROCUnh,
     listTwoClass,
     file=file.path(datapath, "numeric_unhappiness_ROC.Rdata")
)

save(resampsNumSensUnh,
     listTwoClassSens,
     file=file.path(datapath, "numeric_unhappiness_Sens.Rdata")
)

# save variables form best result: ada inROC
bestAdaROC <- listTwoClass$ada$finalModel
#plot and at same time store best variabl
varlist <- varplot(bestAdaROC, type="scores")
varname <- fieldsRange$LABEL[match(names(varlist), fieldsRange$VARIABLE)]

save(bestAdaROC, varlist, varname, file=file.path(datapath, "best_numeric_model_unhappiness_ADA.RData"))

#######################
# stop parallel processing
#######################
stopCluster(cl)
