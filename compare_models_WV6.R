################
# comparing model results for wave 6 (ie singapore) data
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

#get best rf result
load(file.path(datapath, "rftrain_WV6.Rdata"))

#get best rpart result
load(file.path(datapath, "rparttrain_WV6.Rdata"))

#get best party result
load(file.path(datapath, "ctreetrain_WV6.Rdata"))


#compare all

#resample
resampsAll <- resamples(list(rf = fitRf,
                             rpart = fitRpart5,
                             ctree = fitCtree2))

#indiv predicts
testRpart <- predict(fitRpart5, newdata=dnontest)
testRf <- predict(fitRf, newdata=dnontest)
testCtree <- predict(fitCtree2, newdata=dnontest)

comp_models <- function(testset, predictset) {
  compMatch <- testset == predictset$Happiness
  cat(paste("Correct:", sum(compMatch), "/", length(compMatch),
            "(", sum(compMatch)/length(compMatch)*100, "%)"))
}

cat("testRpart ", comp_models(testRpart, dnontest), "\n")
cat("testRf ", comp_models(testRf, dnontest), "\n")
cat("testCtree ", comp_models(testCtree, dnontest), "\n")