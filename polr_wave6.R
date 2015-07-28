################
# analyzing wave 6 (ie singapore) data
# with probit
################


##### NOT WORKING YET

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
sourcefile <- "WV6.RData"
codebook <- "WV6_Codebook_v_2014_11_07.csv"
fieldinfo <- "WVS_6_valuerange.csv"
mainvar <- "WV6"
happinessfield="V10"
countryfield="V2"
cat("Loading data ... ")
d <- load.WVS(sourcefile, codebook, fieldinfo, mainvar, happinessfield, countryfield)
dnon <- merge.nonanswers(d)
cat("done\n")

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(d$Happiness, p=.8, list=FALSE)
dtrain <- d[trainIndex, ]
dtest <- d[-trainIndex, ]
dnontrain <- dnon[trainIndex, ]
dnontest <- dnon[-trainIndex, ]


# hack for polr: suspected polr cannot handle too many factors in input
# dnontrain <- as.data.frame(sapply(dnontrain, as.numeric))
dnontrain[, names(dnontrain) != "Happiness"] <-
  as.data.frame(sapply(dnontrain[, names(dnontrain) != "Happiness"], as.numeric))

# train setting: set in WVS_lib.R
#tuneLength <- 10

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# train 1: Ordinal logistics / probit regression
cat("polr")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitPolr <- train(Happiness ~ ., data = dnontrain,
               method = "polr",
               trControl = fitControl
               # tuneLength = tuneLength
)
time1 <- proc.time()-ptm
cat(time1)


save(fitPolr, file=file.path(datapath, "polrtrain_WV6.Rdata"))
# save(resampsRfWV6, fitRf, file=file.path(datapath, "rftrain_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)         
