################
# doing feature elimination on wave 6 (ie singapore) data
# to identify predictors for unhappiness (instead of happiness)
# using random forest
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

# set Unhappiness
d$Unhappiness <- T
d[which(d$Happiness %in% c("Very happy", "Quite happy")), ]$Unhappiness <- F
d$Unhappiness <- factor(d$Unhappiness)  # caret needs this as factor to train
d$Happiness <- NULL  # eliminate the field to avoid confusion
wt <- table(d$Unhappiness)/nrow(d)  # parameter for class weightage in training below

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(d$Unhappiness, p=.8, list=FALSE)
dtrain <- d[trainIndex, ]
dtest <- d[-trainIndex, ]

#hack for random forest
dtrain[, names(dtrain) != "Unhappiness"] <-
  as.data.frame(sapply(dtrain[, names(dtrain) != "Unhappiness"], as.numeric))

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

##############
# set controls for recursive feature elimination
##############
set.seed(10)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 5,
                   verbose = FALSE)

rfProfile <- rfe(dtrain[, names(dtrain) != "Unhappiness"], dtrain$Unhappiness,
                 sizes = c(2:10, 15, 20, 25, 30, 35, 40, 45, 50),
                 metric = "Accuracy",
                 rfeControl = ctrl,
                 classwt = wt)

#oversample
scaleFactor <- round(wt[1]/wt[2])
dtrue <- dtrain[dtrain$Unhappiness=="TRUE", ]
drepl <- NULL
for (x in 1:scaleFactor) drepl <- rbind(drepl, dtrue)

# add back happiness
drepl <- rbind(drepl, dtrain[dtrain$Unhappiness=="FALSE", ])
 
rfOverProfile <- rfe(drepl[, names(drepl) != "Unhappiness"], drepl$Unhappiness,
                 sizes = c(2:10, 15, 20, 25, 30, 35, 40, 45, 50),
                 metric = "Accuracy",
                 rfeControl = ctrl)

rfUnhappinessFeatures <- rfProfile
rfUnhappinessFeaturesOver <- rfOverProfile

save(rfUnhappinessFeatures, rfUnhappinessFeaturesOver,
     file=file.path(datapath, "rf_features_WV6_Unhappiness.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)   
