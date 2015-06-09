#################
# perform tree regression on longitudinal data
#################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

require(caret)
require(rpart)

# load longitudinal data
d <- load.WVS.long.happy()

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(d$Happiness, p=.8, list=FALSE)
dtrain <- d[trainIndex, ]
dtest <- d[-trainIndex, ]

# train!
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)
fitRpart <- train(Happiness ~ ., data = dtrain,
                method = "rpart",
                trControl = fitControl
                )
fitCHAID <- train(Happiness ~ ., data = dtrain,
                     method = "chaid",
                     trControl = fitControl
)

testRpart <- predict(fitRpart, newdata=dtest)

testCHAID <- predict(fitCHAID, newdata=dtest)

fc <- trainControl(method="cv", number=5)

rpm <- rpart(Happiness ~ ., data = dtrain, control=rpart.control(minsplit=2))
