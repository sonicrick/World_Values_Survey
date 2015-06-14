#################
# compare rpart tree between Finland and Singapore
# forcing all predictors to remain ordinal
#################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

require(caret)
require(rpart)
require(rattle)
require(rpart.plot)


##############
# prepare data if not yet
##############

# load longitudinal data
d <- load.WVS.long.happy()

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(d$Happiness, p=.8, list=FALSE)
dtrain <- d[trainIndex, ]
dtest <- d[-trainIndex, ]

# separate Fin and Sin training set
trainFin <- separate.country(dtrain, "Finland")
trainSin <- separate.country(dtrain, "Singapore")
testFin <- separate.country(dtest, "Finland")
testSin <- separate.country(dtest, "Singapore")

# train setting: set in WVS_lib.R


##############
# enable parallel processing
# NOTE: do this before train model, AFTER setup
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#############
# train model
#############
cat("rpart Fin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
rpartFin <- train(x=trainFin[, names(trainFin) != "Happiness"],
                   y = trainFin$Happiness,
                   method = "rpart",
                   trControl = fitControl
)
accFin <- sum(testFin$Happiness == predict(rpartFin, newdata=testFin)) / nrow(testFin)

cat("rpart Sin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
rpartSin <- train(x=trainSin[, names(trainSin) != "Happiness"],
                  y = trainSin$Happiness,
                  method = "rpart",
                  trControl = fitControl
)
accSin <- sum(testSin$Happiness == predict(rpartSin, newdata=testSin)) / nrow(testSin)

cat("rpart2 Fin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
# rpart2Fin <- train(Happiness ~ ., data = trainFin,
#                    method = "rpart2",
#                    trControl = fitControl
# )
rpart2Fin <- train(x=trainFin[, names(trainFin) != "Happiness"],
                   y = trainFin$Happiness,
                   method = "rpart2",
                   trControl = fitControl
)
accFin2 <- sum(testFin$Happiness == predict(rpart2Fin, newdata=testFin)) / nrow(testFin)

cat("rpart2 Sin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
rpart2Sin <- train(x=trainSin[, names(trainSin) != "Happiness"],
                  y = trainSin$Happiness,
                  method = "rpart2",
                  trControl = fitControl
)
accSin2 <- sum(testSin$Happiness == predict(rpart2Sin, newdata=testSin)) / nrow(testSin)

resamps <- resamples(list(rpartFin=rpartFin,
                          rpartSin=rpartSin,
                          rpart2Fin=rpart2Fin,
                          rpart2Sin=rpart2Sin))

# based on resamps result, rpartFin2 and rpartSin2 are better
#save(resamps, rpart2Fin, rpart2Sin, file=file.path(datapath, "FinvsSin_ordinal_predictors.Rdata"))

prp(rpart2Fin$finalModel)

#rp2 <- rpart(Happiness ~ ., data = trainSin, control=rpart.control(cp=.0125, maxdepth=6))

#######################
# stop parallel processing
#######################
stopCluster(cl)
