################
# analyzing unhappiness of wave 6 (ie Singapore) data using upsampling
# using multiple models for comparison
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

#### NOTE: data processed will be using dnon (i.e. where NA answers are merged -> se load_WV6_for_caret.R)

# manual tweaking of variables to streamline partitioning
# drop "Year of birth" in favour of age
dnon <- dnon[, names(dnon) != "V241"]


# set Unhappiness using dnon 
dnon$Unhappiness <- "Y"
dnon[which(d$Happiness %in% c("Very happy", "Quite happy")), ]$Unhappiness <- "N"
dnon$Unhappiness <- factor(dnon$Unhappiness)  # caret needs this as factor to train
dnon$Happiness <- NULL  # eliminate the field to avoid confusion
wt <- table(dnon$Unhappiness)/nrow(dnon)  # parameter for class weightage in training below

# read field descriptions in codebook
fieldsRange <- read.csv(file.path(datapath, "WVS_6_valuerange.csv"),
                   stringsAsFactors=FALSE)
#keep only fields in dataset
fieldsRange <- fieldsRange[fieldsRange$VARIABLE %in% names(dnon), ]
# lchk <- lapply(fieldsRange$VALUE_RANGE, extract.field.numbers)
# names(lchk) <- fieldsRange$VARIABLE
# maxchk <- sapply(lchk, max)
# codeVar <- names(lchk)[maxchk==-Inf | maxchk > 10]  # those mapped to codes
# scale6Var <- names(lchk)[maxchk==6]  # those with values on scale 1 to 6
# scale6Var <- scale6Var[scale6Var!="V1"]  # hardcoded elimination: V1 is wave indicator, which happens to have 6
# scale10Var <- names(lchk)[maxchk==10]  # those with values on scale 1 to 10
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



# train setting: set in WVS_lib.R
tuneLength <- 5


##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# convenience function for training unhappiness
trainUnhappiness <- function(tmethod, tdata=dnumTrain, control=fitControl, tmetric="Accuracy", display=tmethod) {
  cat(paste0(display, "\n"))
  set.seed(12345) # need to set same seed for all training to have same fold separation?
  ptm <- proc.time()
  fitModel <- train(Unhappiness ~ ., data = tdata,
                    method = tmethod,
                    trControl = control,
                    tuneLength = tuneLength,
                    metric=tmetric
  )
  time1 <- proc.time()-ptm
  cat(time1)
  return(fitModel)
}

models <- c(
  # tried and eliminated because errors
  # "lm", "gam", "knn",
  "ctree", "rpart", "ada", "C5.0", "J48", "lda", "LMT", "nb", "rocc"
  )
# train 1: linear
# unh_lm <- trainUnhappiness("lm")
unh_ctree <- trainUnhappiness("ctree")
unh_rpart <- trainUnhappiness("rpart")
unh_ada <- trainUnhappiness("ada")
unh_c50 <- trainUnhappiness("C5.0")
unh_J48 <- trainUnhappiness("J48")
unh_lda <- trainUnhappiness("lda")
unh_lmt <- trainUnhappiness("LMT")
unh_nb <- trainUnhappiness("nb")
unh_rocc <- trainUnhappiness("rocc")
# unh_gam <- trainUnhappiness("gam")
# unh_knn <- trainUnhappiness("knn")

# train setting for Binary classifier using sensitivity and specificity
fitTwoClass <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5,
  classProbs=TRUE,
  summaryFunction = twoClassSummary
  )

require(pROC)
listTwoClass <- lapply(models, function(x) trainUnhappiness(tmethod=x, control=fitTwoClass, tmetric="ROC" ))
tst <- trainUnhappiness(tmethod="ctree", control=fitTwoClass, tmetric="ROC")

listFits <- list( #unh_lm=unh_lm,
  unh_ctree=unh_ctree,
  unh_rpart=unh_rpart,
  unh_ada=unh_ada,
  unh_c50=unh_c50,
  unh_J48=unh_J48,
  unh_lda=unh_lda,
  unh_lmt=unh_lmt,
  unh_nb=unh_nb,
  unh_rocc=unh_rocc #,
  # unh_gam=unh_gam,
  # unh_knn=unh_knn
)

# resampsNumUnh <- resamples(list( #unh_lm=unh_lm,
#                               unh_ctree=unh_ctree,
#                               unh_rpart=unh_rpart,
#                               unh_ada=unh_ada,
#                               unh_c50=unh_c50,
#                               unh_J48=unh_J48,
#                               unh_lda=unh_lda,
#                               unh_lmt=unh_lmt,
#                               unh_nb=unh_nb,
#                               unh_rocc=unh_rocc #,
#                               # unh_gam=unh_gam,
#                               # unh_knn=unh_knn
#                               )
#                          )

resampsNumUnh <- resamples(listFits)

save(#unh_lm,
  unh_ctree,
  unh_rpart,
  unh_ada,
  unh_c50,
  unh_J48,
  unh_lda,
  unh_lmt,
  unh_nb,
  unh_rocc,
  # unh_gam,
  # unh_knn,
  resampsNumUnh,
  file=file.path(datapath, "numeric_unhappiness.Rdata")
)

accList <- lapply(listFits, accuracy_chk, dnumTest, "Unhappiness")
allAccur <- sapply(accList, function(x) x$accur)
unhAccur <- sapply(accList, function(x) x$crosstab[2,2]/sum(x$crosstab[2,]))
unhRecall <- sapply(accList, function(x) x$crosstab[2,2]/sum(x$crosstab[,2]))

save(resampsNumUnh,
     listFits,
     file=file.path(datapath, "numeric_unhappiness.Rdata")
     )

#######################
# stop parallel processing
#######################
stopCluster(cl)
