require(caret)
require(rpart)

load("trainFin.RData")

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats = 5)

#setup parallel processing
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#train
set.seed(12345)
ptm <- proc.time()
system.time(binarysplits <- train(Happiness ~ ., data = trainFin2,
                   method = "rpart2",
                   trControl = fitControl
))
time1 <- proc.time()-ptm
cat(time1)

set.seed(12345)
ptm <- proc.time()
system.time(ordinalsplits <- train(x=trainFin2[, names(trainFin2) != "Happiness"],
                   y = trainFin2$Happiness,
                   method = "rpart2",
                   trControl = fitControl
))
time2 <- proc.time()-ptm
cat(time2)


stopCluster(cl)
