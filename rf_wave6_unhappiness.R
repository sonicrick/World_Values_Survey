################
# analyzing wave 6 (ie singapore) data for unhappy vs happy
# with randomforest
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
# dnontrain <- dnon[trainIndex, ]
# dnontest <- dnon[-trainIndex, ]

# train setting: set in WVS_lib.R
tuneLength <- 5

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# train 1: basic random forest
cat("Random forest")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitRfBinary <- train(Unhappiness ~ ., data = dtrain,
                   method = "rf",
                   classwt = wt,  # to try address unbalanced observation
                   trControl = fitControl,
                   tuneLength = tuneLength
)
time1 <- proc.time()-ptm
cat(time1)

# train 2: random forest with top 35 features from rle
cat("Random forest with eliminated features")
load(file.path(datapath, "rf_features_WV6_Unhappiness.Rdata"))
drleTrain <- dtrain[, names(dtrain) %in% c(rfUnhappinessFeatures$optVariables,
                                           "Unhappiness")]
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitRfRleBinary <- train(Unhappiness ~ ., data = drleTrain,
                     method = "rf",
                     classwt = wt,  # to try address unbalanced observation
                     trControl = fitControl,
                     tuneLength = tuneLength
)
time2 <- proc.time()-ptm
cat(time2)


# train 3: random forest with top 35 features from rle
# and replicated data to even up unhappiness
cat("Random forest with unskewed data and eliminated features")

# multiply unhappy records to eliminate unbalanced in data
scaleFactor <- round(wt[1]/wt[2])
dtrue <- drleTrain[drleTrain$Unhappiness=="TRUE", ]  # drleTrain loaded in train 2 above
drepl <- NULL
for (x in 1:scaleFactor) drepl <- rbind(drepl, dtrue)

# add back happiness
drepl <- rbind(drepl, drleTrain[drleTrain$Unhappiness=="FALSE", ])

set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitRfRleReplBinary <- train(Unhappiness ~ ., data = drepl,
                        method = "rf",
                        trControl = fitControl,
                        tuneLength = tuneLength
)
time3 <- proc.time()-ptm
cat(time3)


# train 4: random forest with replicated data to even up unhappiness
#### NOTE: BAD RESULT, TRIVIAL PREDICTION WHERE ALL SET TO FALSE, RETAINED HERE AS NOTE FOR REPRODUCIBILITY
cat("Random forest with unskewed data and full features")

# multiply unhappy records to eliminate unbalanced in data
scaleFactor <- round(wt[1]/wt[2])
dtrue <- dtrain[dtrain$Unhappiness=="TRUE", ]  # dtrain as loaded in train 1 above
dreplfull <- NULL
for (x in 1:scaleFactor) dreplfull <- rbind(dreplfull, dtrue)

# add back happiness
dreplfull <- rbind(dreplfull, dtrain[dtrain$Unhappiness=="FALSE", ])

set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitRfReplBinary <- train(Unhappiness ~ ., data = dreplfull,
                            method = "rf",
                            trControl = fitControl,
                            tuneLength = tuneLength
)
time4 <- proc.time()-ptm
cat(time4)


# train 5: random forest with top 35 features from oversampled rle of Unhappiness (as opposed to normal sample)
# and replicated data to even up unhappiness
cat("Random forest with unskewed data and eliminated oversampled features")

# select only top 35 fields
drleOverTrain <- dtrain[, names(dtrain) %in% c(rfUnhappinessFeaturesOver$optVariables[1:35],
                                           "Unhappiness")]

# multiply unhappy records to eliminate unbalanced in data
scaleFactor <- round(wt[1]/wt[2])
dtrue <- drleOverTrain[drleOverTrain$Unhappiness=="TRUE", ]
dreplOver <- NULL
for (x in 1:scaleFactor) dreplOver <- rbind(dreplOver, dtrue)

# add back happiness
dreplOver <- rbind(dreplOver, drleOverTrain[drleOverTrain$Unhappiness=="FALSE", ])

set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitRfRleReplOverBinary <- train(Unhappiness ~ ., data = dreplOver,
                            method = "rf",
                            trControl = fitControl,
                            tuneLength = tuneLength
)
time5 <- proc.time()-ptm
cat(time5)


# # train 5: using glmnet and oversampling
# require(glmnet)
# cat("glmnet")
# set.seed(12345) # need to set same seed for all training to have same fold separation?
# ptm <- proc.time()
# fitGlmReplBinary <- train(Unhappiness ~ ., data = dreplfull,
#                          method = "glm",
#                          trControl = fitControl,
#                          tuneLength = tuneLength
# )
# time5 <- proc.time()-ptm
# cat(time5)


# compare all

resampsRfBinaryWV6 <- resamples(list(rfNormalBinary = fitRfBinary,
                              rfTopFeaturesBinary = fitRfRleBinary,
                              rfTopFeaturesReplicateBinary = fitRfRleReplBinary,
                              rfReplicateBinary = fitRfReplBinary,
                              rfTopFeaturesReplicateBinaryOver = fitRfRleReplOverBinary
                              # GlmReplBinary = fitGlmReplBinary)
                              )
                              )

save(fitRfBinary, fitRfRleBinary, fitRfRleReplBinary, fitRfReplBinary, fitRfRleReplOverBinary, resampsRfBinaryWV6, file=file.path(datapath, "rf_binary_train_WV6.Rdata"))
# save(resampsRfWV6, fitRf, file=file.path(datapath, "rftrain_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)         
