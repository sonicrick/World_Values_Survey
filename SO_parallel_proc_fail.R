require(caret)
require(rpart)

load("trainFin.RData")

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

#setup parallel processing
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#train
set.seed(12345)
ptm <- proc.time()
# system.time(firstSet <- train(Happiness ~ ., data = trainFin1,
#                               method = "rpart2", trControl = fitControl))
firstSet <- train(x=trainFin1[, names(trainFin1) != "Happiness"],
                  y = trainFin1$Happiness,
                  method = "rpart2", trControl = fitControl)

time1 <- proc.time()-ptm
cat(time1)

set.seed(12345)
ptm <- proc.time()
secondSet <- train(x=trainFin2[, names(trainFin2) != "Happiness"],
                   y = trainFin2$Happiness,
                   method = "rpart2",
                   trControl = fitControl)
time2 <- proc.time()-ptm
cat(time2)

stopCluster(cl)
