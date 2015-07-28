################
# analyzing wave 6 (ie Singapore) data
# with randomforest, exclude quite Happy
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

#only run on those not Quite Happy
dnontrain <- dnontrain[dnontrain$Happiness != "Quite happy", ]
dnontrain$Happiness <- factor(dnontrain$Happiness)

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
fitRfSin_exclQH <- train(Happiness ~ ., data = dnontrain,
                   method = "rf",
                   trControl = fitControl,
                   tuneLength = tuneLength
)
time1 <- proc.time()-ptm
cat(time1)

##############
# set controls for recursive feature elimination
##############
#hack for random forest
# dnontrain <- as.data.frame(sapply(dnontrain, as.numeric))
dnontrain[, names(dnontrain) != "Happiness"] <-
  as.data.frame(sapply(dnontrain[, names(dnontrain) != "Happiness"], as.numeric))

set.seed(10)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 5,
                   verbose = FALSE)

rfProfileSin_exclQH <- rfe(dnontrain[, names(dnontrain) != "Happiness"], dnontrain$Happiness,
                    sizes = c(2:10, 15, 20, 25, 30, 35, 40, 45, 50),
                    metric = "Accuracy",
                    rfeControl = ctrl)

rfProfileSin_exclQH$optVariables

# resampsRfWV5 <- resamples(list(rfcleannonanswer = fitRfFin,
#                               rfWeightcleannonanswer = fitRfFinWeights))
#  
save(fitRfSin_exclQH, rfProfileSin_exclQH, file=file.path(datapath, "rftrain_WV6_exclQH.Rdata"))


#######################
# stop parallel processing
#######################
stopCluster(cl)         
