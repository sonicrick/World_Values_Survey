################
# analyzing wave 6 (ie singapore) data
# with randomforest and only top 50 features of what is found by
# rf_feature_elim_WV6.R
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

# load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

# load feature elimination result, result in rfProfile
load(file=file.path(datapath, "rf_rfe_WV6.Rdata"))

#get top 50 features
xnames <- rfProfile$optVariables

#construct formula
formulaList <- paste("Happiness ~",paste(xnames, collapse="+"))

#alternate apporach: reduce dataframe size
d50 <- dnontrain[, which(names(dnontrain) %in% c(xnames, "Happiness"))]

# train setting: set in WVS_lib.R
#tuneLength <- 10

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# train 1: basic random forest
cat("Random forest top 50 features")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
# fitRf50 <- train(as.formula(formulaList),
#                  data = dnontrain,
#                method = "rf",
#                trControl = fitControl
#                # tuneLength = tuneLength
#                )

#alternate apporach: reduce dataframe size
fitRf50 <- train(Happiness ~ .,
                 data = d50,
                 method = "rf",
                 trControl = fitControl
                 # tuneLength = tuneLength
)


time1 <- proc.time()-ptm
cat(time1)

# train 2: Boruta random forest - too long
# cat("Boruta")
# set.seed(12345) # need to set same seed for all training to have same fold separation?
# ptm <- proc.time()
# fitBoruta <- train(Happiness ~ ., data = dnontrain,
#                    method = "Boruta",
#                    trControl = fitControl
#                    # tuneLength = tuneLength
# )
# time2 <- proc.time()-ptm
# cat(time2)
# 
# resampsRfWV6 <- resamples(list(rfcleannonanswer = fitRf,
#                               borutacleannonanswer = fitBoruta))

#save(fitRf50, file=file.path(datapath, "rf50train_WV6.Rdata"))
# save(resampsRfWV6, fitRf, file=file.path(datapath, "rftrain_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)         
