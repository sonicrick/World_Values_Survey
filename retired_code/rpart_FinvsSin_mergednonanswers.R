#################
# compare rpart tree between Finland and Singapore
# note: all ordinal predictors converted to dummy categories by caret
# same as rpart_FinvsSin.R but with non-answers grouped together
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
d <- merge.nonanswers(d)

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

# rpFin <- rpart(Happiness ~ ., data = trainFin) #, control=rpart.control(minsplit=2))
# rpSin <- rpart(Happiness ~ ., data = trainSin) #, control=rpart.control(minsplit=2))
# rpFin2 <- rpart(Happiness ~ ., data = trainFin, control=rpart.control(minsplit=2, minbucket=1))
# rpSin2 <- rpart(Happiness ~ ., data = trainSin, control=rpart.control(minsplit=2, minbucket=1))
# rpFin3 <- rpart(Happiness ~ ., data = trainFin, control=rpart.control(minsplit=2, minbucket=1, cp=.005))


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
rpartFin <- train(Happiness ~ ., data = trainFin,
                  method = "rpart",
                  trControl = fitControl
)
accFin <- sum(testFin$Happiness == predict(rpartFin, newdata=testFin)) / nrow(testFin)

cat("rpart Sin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
rpartSin <- train(Happiness ~ ., data = trainSin,
                  method = "rpart",
                  trControl = fitControl
)
accSin <- sum(testSin$Happiness == predict(rpartSin, newdata=testSin)) / nrow(testSin)

cat("rpart2 Fin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
rpart2Fin <- train(Happiness ~ ., data = trainFin,
                   method = "rpart2",
                   trControl = fitControl
)

accFin2 <- sum(testFin$Happiness == predict(rpart2Fin, newdata=testFin)) / nrow(testFin)

cat("rpart2 Sin \n")
set.seed(12345) # need to set same seed for all training to have same fold separation
rpart2Sin <- train(Happiness ~ ., data = trainSin,
                   method = "rpart2",
                   trControl = fitControl
)
accSin2 <- sum(testSin$Happiness == predict(rpart2Sin, newdata=testSin)) / nrow(testSin)

resamps <- resamples(list(rpartFin=rpartFin,
                          rpartSin=rpartSin,
                          rpart2Fin=rpart2Fin,
                          rpart2Sin=rpart2Sin))

# based on resamps result, rpartFin2 and rpartSin2 are better
#save(resamps, rpart2Fin, rpart2Sin, file=file.path(datapath, "FinvsSin.Rdata"))

prp(rpart2Fin$finalModel)

#rp2 <- rpart(Happiness ~ ., data = trainSin, control=rpart.control(cp=.0125, maxdepth=6))

#######################
# stop parallel processing
#######################
stopCluster(cl)
