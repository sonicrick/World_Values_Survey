################
# doing feature elimination on wave 6 (ie singapore) data
# using random forest
################

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

#hack for random forest
# dnontrain <- as.data.frame(sapply(dnontrain, as.numeric))
dnontrain[, names(dnontrain) != "Happiness"] <-
  as.data.frame(sapply(dnontrain[, names(dnontrain) != "Happiness"], as.numeric))

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

##############
# set controls for recursive feature elimination
##############
set.seed(10)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 5,
                   verbose = FALSE)

rfProfile <- rfe(dnontrain[, names(dnontrain) != "Happiness"], dnontrain$Happiness,
                 sizes = c(2:10, 15, 20, 25, 30, 35, 40, 45, 50),
                 metric = "Accuracy",
                 rfeControl = ctrl)


#######################
# stop parallel processing
#######################
stopCluster(cl)   