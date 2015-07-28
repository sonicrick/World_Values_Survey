################
# analyzing wave 6 (ie singapore) data
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

# train setting: set in WVS_lib.R
tuneLength <- 10

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# train 1: basic rpart 1
cat("Rpart 1")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitRpart1 <- train(Happiness ~ ., data = dtrain,
                   method = "rpart",
                   trControl = fitControl,
                   tuneLength = tuneLength
)

# train 2: basic rpart2
cat("Rpart 2")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitRpart2 <- train(Happiness ~ ., data = dtrain,
                   method = "rpart2",
                   trControl = fitControl,
                   tuneLength = tuneLength
)

# # train 3: rpart2 ordinal
# cat("Rpart 3")
# set.seed(12345) # need to set same seed for all training to have same fold separation?
# fitRpart3 <- train(x=dtrain[, names(dtrain)!="Happiness"], y=dtrain$Happiness,
#                    method = "rpart2",
#                    trControl = fitControl
# )

# train 4: rpart 1 with merged nonanswers
cat("Rpart 1")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitRpart4 <- train(Happiness ~ ., data = dnontrain,
                   method = "rpart",
                   trControl = fitControl,
                   tuneLength = tuneLength
)

# train 5: rpart2 with merged nonanswers
cat("Rpart 2")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitRpart5 <- train(Happiness ~ ., data = dnontrain,
                   method = "rpart2",
                   trControl = fitControl,
                   tuneLength = tuneLength
)

resampsWV6 <- resamples(list(rpart1 = fitRpart1,
                             rpart2 = fitRpart2,
                             rpart1cleannonanswer = fitRpart4,
                             rpart2cleannonanswer = fitRpart5))

# save(resampsWV6, fitRpart2, fitRpart5, file=file.path(datapath, "rparttrain_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)         