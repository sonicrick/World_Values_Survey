################
# analyzing unhappiness of wave 6 (ie Singapore) data using upsampling
# using LDA, ADA and XGB
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')
require(plyr); require(dplyr)
require(xgboost)

#### NOTE: data processed will be using dnon (i.e. where NA answers are merged -> se load_WV6_for_caret.R)

# manual tweaking of variables to streamline partitioning
# drop "Year of birth" in favour of age
dnon <- dnon[, names(dnon) != "V241"]


# set Unhappiness using dnon 
dnon$Unhappiness <- "Y"
dnon[which(d$Happiness %in% c("Very happy", "Quite happy")), ]$Unhappiness <- "N"
dnon$Unhappiness <- relevel(factor(dnon$Unhappiness), ref="Y")  # caret needs this as factor to train
dnon$Happiness <- NULL  # eliminate the field to avoid confusion
wt <- table(dnon$Unhappiness)/nrow(dnon)  # parameter for class weightage in training below

# read field descriptions in codebook
fieldsRange <- read.csv(file.path(datapath, "WVS_6_valuerange.csv"),
                        stringsAsFactors=FALSE)
#keep only fields in dataset
fieldsRange <- fieldsRange[fieldsRange$VARIABLE %in% names(dnon), ]
numScale <- fieldsRange$VARIABLE[fieldsRange$NonStandard!="Y"]

# convert numeric field in-situ
dmixnum <- dnon
mixnumIdx <-  which(names(dnon) %in% numScale)
dmixnum[, mixnumIdx] <- lapply(dnon[, mixnumIdx], function(x) as.numeric(as.character(x)))
# dnum <- as.data.frame(sapply(dnon[, (names(dnon) %in% numScale)], as.numeric))
# dcat <- dnon[, !((names(dnon) %in% numScale))]
# 
# # do preprocessing for numerical
# preProcNum <- preProcess(dnum, method=c("range"))
# dnum <- predict(preProcNum, dnum)
# dnum <- cbind(dnum, Unhappiness=dnon$Unhappiness)

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(dnon$Unhappiness, p=.8, list=FALSE)
dnontrain <- dnon[trainIndex, ]
dnontrain <- upSample(dnontrain, dnontrain$Unhappiness, list=T)[[1]]
dnontest <- dnon[-trainIndex, ]

dmixnumTrain <- dmixnum[trainIndex, ]
dmixnumTrain <- upSample(dmixnumTrain, dmixnumTrain$Unhappiness, list=T)[[1]]
dmixnumTest <- dmixnum[-trainIndex, ]


##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

models <- c(
  # tried and eliminated because errors
  # "xgbLinear", "ada", "C5.0", 
  # tried and eliminated because performance not better than C5.0,
  # and results cannot be saved in RData (it links to environment)
  # ,
  "xgbTree", "rf"
)

# train 1: optimize by Accuracy (default)
listAllUnhAcc <- lapply(models, function(x) trainUnhappiness(tmethod=x, tdata=dnontrain))

names(listAllUnhAcc) <- models
resampsAllUnh <- resamples(listAllUnhAcc)

save(resampsAllUnh,
     listAllUnhAcc,
     file=file.path(datapath, "all_unhappiness.Rdata")
)

# train 2: optimize by Accuracy for fields mixed(numeric and factor)
listAllMixAcc <- lapply(models, function(x) trainUnhappiness(tmethod=x, tdata=dmixnumTrain))

names(listAllMixAcc) <- models
resampsAllMix <- resamples(listAllMixAcc)

save(resampsAllMix,
     listAllMixAcc,
     file=file.path(datapath, "all_mix_unhappiness.Rdata")
)

# # train 2: by ROC
# require(pROC)
# listAllTwoClass <- lapply(models, function(x)
#   trainUnhappiness(tmethod=x, tdata=dnontrain, control=fitTwoClass, tmetric="ROC" ))
# 
# names(lisAlltTwoClass) <- models
# resampsAllROCUnh <- resamples(listAllTwoClass)
# 
# save(resampsAllROCUnh,
#      listAllTwoClass,
#      file=file.path(datapath, "all_unhappiness_ROC.Rdata")
# )
# 
# # train 3: by sensitivity
# # NOTE: from past runs, observed that only a few methods have different outcome optimizing for sensitivity vs for ROC
# listAllTwoClassSens <- lapply(models, function(x)
#   trainUnhappiness(tmethod=x, tdata=dnontrain, control=fitTwoClass, tmetric="Sens" ))
# # tst <- trainUnhappiness(tmethod="rocc", control=fitTwoClass, tmetric="ROC")
# 
# names(listTwoClassSens) <- models
# resampsAllSensUnh <- resamples(listAllTwoClassSens)
# 
# save(resampsAllSensUnh,
#      listAllTwoClassSens,
#      file=file.path(datapath, "all_unhappiness_Sens.Rdata")
# )



accListAll <- lapply(listAllUnhAcc, accuracy_chk, dnontest, "Unhappiness")
allAccurAll <- sapply(accListAll, function(x) x$accur)
unhAccurAll <- sapply(accListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhRecallAll <- sapply(accListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

#display
allAccurAll;unhAccurAll;unhRecallAll

accListAllMix <- lapply(listAllMixAcc, accuracy_chk, dmixnumTest, "Unhappiness")
allAccurAllMix <- sapply(accListAllMix, function(x) x$accur)
unhAccurAllMix <- sapply(accListAllMix, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhRecallAllMix <- sapply(accListAllMix, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

#display
allAccurAllMix;unhAccurAllMix;unhRecallAllMix

# ROCListAll <- lapply(listAllTwoClass, accuracy_chk, dnontest, "Unhappiness")
# allROCAccurAll <- sapply(ROCListAll, function(x) x$accur)
# unhROCAccurAll <- sapply(ROCListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
# unhROCRecallAll <- sapply(ROCListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))
# 
# SensListAll <- lapply(listAllTwoClassSens, accuracy_chk, dnontest, "Unhappiness")
# allSensAccurAll <- sapply(SensListAll, function(x) x$accur)
# unhSensAccurAll <- sapply(SensListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
# unhSensRecallAll <- sapply(SensListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

# shortlist_model <- c("lda", "rpart", "ada", "C5.0", "nb")
shortlist_model <- models
shortlist <- lapply(shortlist_model, function(x) accListAll[[x]]$crosstab)
names(shortlist) <- shortlist_model
# shortlist <- ROCList[[shortlist_model]]$crosstab

#save all




#######################
# stop parallel processing
#######################
stopCluster(cl)
