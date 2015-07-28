################
# doing feature elimination on wave 6 (ie singapore) data
# using lm
################

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

#hack for random forest
# dnontrain <- as.data.frame(sapply(dnontrain, as.numeric))
train_happiness <- dnontrain$Happiness
dnontrain <- as.data.frame(sapply(dnontrain[, names(dnontrain) != "Happiness"], as.numeric))

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

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 5,
                   verbose = FALSE)

lmProfile <- rfe(dnontrain[, names(dnontrain) != "Happiness"], train_happiness,
                 sizes = c(2:10, 15, 20, 25, 30, 35, 40, 45, 50),
                 metric = "Accuracy",
                 rfeControl = ctrl)

lmProfile$optVariables

save(lmProfile, file=file.path(datapath, "lm_rfe_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)   