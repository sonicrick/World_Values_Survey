################
# analyzing unhappiness of wave 6 (ie Singapore) data using upsampling
# using categoric-scale predictors for comparison
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

# # do preprocessing for numerical
# preProcNum <- preProcess(dnum, method=c("range"))
# dnum <- predict(preProcNum, dnum)
# dnum <- cbind(dnum, Unhappiness=dnon$Unhappiness)

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(dnon$Unhappiness, p=.8, list=FALSE)
dnontrain <- dnon[trainIndex, ]
dnontrain <- upSample(dnontrain, dnontrain$Unhappiness)
dnontest <- dnon[-trainIndex, ]

dcatTrain <- dcat[trainIndex, ]
dcatTrain <- upSample(dcatTrain, dcatTrain$Unhappiness, list=T)[[1]]
dcatTest <- dcat[-trainIndex, ]


##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

models <- c(
  # tried and eliminated because errors
  # "lm", "gam", "knn", "rocc", "lda", 
  # tried and eliminated because performance not better than C5.0,
  # and results cannot be saved in RData (it links to environment)
  # "J48", "LMT",
  "nb", "svmLinear", "ctree", "rpart", "ada", "C5.0"
)

# train 1: optimize by Accuracy (default)
listUnhAccCat <- lapply(models, function(x) trainUnhappiness(tmethod=x, tdata=dcatTrain, control=fitControl))

# train 2: by ROC
require(pROC)
listTwoClassCat <- lapply(models, function(x) trainUnhappiness(tmethod=x, tdata=dcatTrain, control=fitTwoClass, tmetric="ROC" ))
# tst <- trainUnhappiness(tmethod="rocc", control=fitTwoClass, tmetric="ROC")

# train 3: by sensitivity
# NOTE: from past runs, observed that only a few methods have different outcome optimizing for sensitivity vs for ROC
listTwoClassSensCat <- lapply(models, function(x) trainUnhappiness(tmethod=x, tdata=dcatTrain, control=fitTwoClass, tmetric="Sens" ))


#consolidating and saving
names(listUnhAccCat) <- models
resampsNumUnhCat <- resamples(listUnhAccCat)

names(listTwoClassCat) <- models
resampsNumROCUnhCat <- resamples(listTwoClassCat)

names(listTwoClassSensCat) <- models
resampsNumSensUnhCat <- resamples(listTwoClassSensCat)

accListCat <- lapply(listUnhAccCat, accuracy_chk, dcatTest, "Unhappiness")
allAccurCat <- sapply(accListCat, function(x) x$accur)
unhAccurCat <- sapply(accListCat, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhRecallCat <- sapply(accListCat, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

ROCListCat <- lapply(listTwoClassCat, accuracy_chk, dcatTest, "Unhappiness")
allROCAccurCat <- sapply(ROCListCat, function(x) x$accur)
unhROCAccurCat <- sapply(ROCListCat, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhROCRecallCat <- sapply(ROCListCat, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

SensListCat <- lapply(listTwoClassSensCat, accuracy_chk, dcatTest, "Unhappiness")
allSensAccurCat <- sapply(SensListCat, function(x) x$accur)
unhSensAccurCat <- sapply(SensListCat, function(x) x$crosstab[1,1]/sum(x$crosstab[1,]))
unhSensRecallCat <- sapply(SensListCat, function(x) x$crosstab[1,1]/sum(x$crosstab[,1]))

shortlist_model <- c("lda", "rpart", "ada", "C5.0", "nb")
shortlist <- lapply(shortlist_model, function(x) ROCListCat[[x]]$crosstab)
shortlist_sens <- lapply(shortlist_model, function(x) SensListCat[[x]]$crosstab)
names(shortlist) <- shortlist_model
names(shortlist_sens) <- shortlist_model
# shortlist <- ROCList[[shortlist_model]]$crosstab

save(resampsNumUnhCat,
     listUnhAccCat,
     file=file.path(datapath, "categoric_unhappiness.Rdata")
)

save(resampsNumROCUnhCat,
     listTwoClassCat,
     file=file.path(datapath, "categoric_unhappiness_ROC.Rdata")
)

save(resampsNumSensUnhCat,
     listTwoClassSensCat,
     file=file.path(datapath, "categoric_unhappiness_Sens.Rdata")
)


#######################
# stop parallel processing
#######################
stopCluster(cl)