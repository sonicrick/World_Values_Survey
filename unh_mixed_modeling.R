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
# wt <- table(dnon$Unhappiness)/nrow(dnon)  # parameter for class weightage in training below
# 
# # read field descriptions in codebook
# fieldsRange <- read.csv(file.path(datapath, "WVS_6_valuerange.csv"),
#                         stringsAsFactors=FALSE)
# #keep only fields in dataset
# fieldsRange <- fieldsRange[fieldsRange$VARIABLE %in% names(dnon), ]
# numScale <- fieldsRange$VARIABLE[fieldsRange$NonStandard!="Y"]
# 
# # split into numeric and non-numeric
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

# dnumTrain <- dnum[trainIndex, ]
# dnumTrain <- upSample(dnumTrain, dnumTrain$Unhappiness, list=T)[[1]]
# dnumTest <- dnum[-trainIndex, ]


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
  "rf", "xgbTree"
)

# train 1: optimize by Accuracy (default)
listAllUnhAcc <- lapply(models, function(x) trainUnhappiness(tmethod=x, tdata=dnontrain))

# train 2: by ROC
require(pROC)
listAllTwoClass <- lapply(models, function(x)
  trainUnhappiness(tmethod=x, tdata=dnontrain, control=fitTwoClass, tmetric="ROC" ))

# train 3: by sensitivity
# NOTE: from past runs, observed that only a few methods have different outcome optimizing for sensitivity vs for ROC
listAllTwoClassSens <- lapply(models, function(x)
  trainUnhappiness(tmethod=x, tdata=dnontrain, control=fitTwoClass, tmetric="Sens" ))
# tst <- trainUnhappiness(tmethod="rocc", control=fitTwoClass, tmetric="ROC")

#consolidating and saving
names(listAllUnhAcc) <- models
resampsAllUnh <- resamples(listAllUnhAcc)

names(listTwoClass) <- models
resampsAllROCUnh <- resamples(listAllTwoClass)

names(listTwoClassSens) <- models
resampsAllSensUnh <- resamples(listAllTwoClassSens)

accListAll <- lapply(listAllUnhAcc, accuracy_chk, dnontest, "Unhappiness")
allAccurAll <- sapply(accListAll, function(x) x$accur)
unhAccurAll <- sapply(accListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhRecallAll <- sapply(accListAll, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

#display
allAccurAll;unhAccurAll;unhRecallAll

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
save(resampsAllUnh,
     listAllUnhAcc,
     file=file.path(datapath, "all_unhappiness.Rdata")
)

# save(resampsAllROCUnh,
#      listAllTwoClass,
#      file=file.path(datapath, "all_unhappiness_ROC.Rdata")
# )
# 
# save(resampsAllSensUnh,
#      listAllTwoClassSens,
#      file=file.path(datapath, "all_unhappiness_Sens.Rdata")
# )


#######################
# stop parallel processing
#######################
stopCluster(cl)
