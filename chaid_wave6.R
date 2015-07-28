################
# analyzing wave 6 (ie singapore) data
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

require(CHAID)

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

# train setting: set in WVS_lib.R
#tuneLength <- 10

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# train 1: basic chaid
cat("CHAID")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitCHAID <- train(Happiness ~ ., data = dnontrain,
                   method = "chaid",
                   trControl = fitControl
                   # tuneLength = tuneLength
)


# resampsWV6 <- resamples(list(rpart1 = fitRpart1,
#                              rpart2 = fitRpart2,
#                              rpart1cleannonanswer = fitRpart4,
#                              rpart2cleannonanswer = fitRpart5))
# 
# save(resampsWV6, file=file.path(datapath, "CHAIDtrain_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)         
