#################
# compare rpart tree between Finland and Singapore
#################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

require(caret)
require(rpart)
require(MASS)

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
separate.country <- function(wvsdata, country) {
  sepdata <- wvsdata[wvsdata$Country==country, ]
  country.fields <- c("Country", "S003A", "S024", "S025")
  sepdata <- sepdata[, !(names(sepdata) %in% country.fields)]
  return(sepdata)
}

trainFin <- separate.country(dtrain, "Finland")
trainSin <- separate.country(dtrain, "Singapore")
testFin <- separate.country(dtest, "Finland")
testSin <- separate.country(dtest, "Singapore")

rpFin <- rpart(Happiness ~ ., data = trainFin) #, control=rpart.control(minsplit=2))
rpSin <- rpart(Happiness ~ ., data = trainSin) #, control=rpart.control(minsplit=2))
rpFin2 <- rpart(Happiness ~ ., data = trainFin, control=rpart.control(minsplit=2, minbucket=1))
rpSin2 <- rpart(Happiness ~ ., data = trainSin, control=rpart.control(minsplit=2, minbucket=1))
rpFin3 <- rpart(Happiness ~ ., data = trainFin, control=rpart.control(minsplit=2, minbucket=1, cp=.005))


# train setting
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5)

#train model
fitRpartFin <- train(Happiness ~ ., data = trainFin,
                   method = "rpart",
                   trControl = fitControl
)
accFin <- sum(testFin$Happiness == predict(fitRpartFin, newdata=testFin)) / nrow(testFin)

fitRpartSin <- train(Happiness ~ ., data = trainSin,
                     method = "rpart",
                     trControl = fitControl
)
accSin <- sum(testSin$Happiness == predict(fitRpartSin, newdata=testSin)) / nrow(testSin)