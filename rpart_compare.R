#################
# compare different rpart aprroaches on WVS longitudinal data
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

# train setting
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5)

# train 1: basic rpart 1
cat("Rpart 1")
fitRpart1 <- train(Happiness ~ ., data = dtrain,
                  method = "rpart",
                  trControl = fitControl
                  )

#train 2: ordinal logistic regression
# NOT WORKING YET
cat("Polr")
fitPolr <-  train(Happiness ~ ., data = dtrain,
                  method = "polr",
                  trControl = fitControl
                  )

# train 3: basic rpart2
cat("Rpart 2")
fitRpart2 <- train(Happiness ~ ., data = dtrain,
                   method = "rpart2",
                   trControl = fitControl
)

# train 4: rpart with categorical (not ordinal) input
# NOT WORKING YET error at train
# create pure categorical data
dc <- predict(dummyVars(Happiness ~ ., d), d)
dcat <- cbind(d$Happiness, as.data.frame(dc))
names(dcat)[1] <- "Happiness"
dcatTrain <- dcat[trainIndex, ]
dcatTest <- dcat[-trainIndex, ]

fitRpartCat <- train(Happiness ~ ., data = dcatTrain,
                     method = "rpart",
                     trControl = fitControl
)

resamps <- resamples(list(rpart1 = fitRpart1,
                          rpart2 = fitRpart2))

rp2 <- rpart(Happiness ~ ., data = dtrain, control=rpart.control(minsplit=2))
