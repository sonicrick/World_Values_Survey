################
# analyzing wave 6 (ie singapore) data
# with ctree from party package
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

require(party)

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
tuneLength <- 10

##############
# enable parallel processing
##############
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

# train 1: basic ctree
cat("ctree")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitCtree1 <- train(Happiness ~ ., data = dnontrain,
                   method = "ctree",
                   trControl = fitControl,
                   tuneLength = tuneLength
)
time1 <- proc.time()-ptm
cat(time1)

# train 2: basic ctree2
cat("ctree2")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fitCtree2 <- train(Happiness ~ ., data = dnontrain,
                  method = "ctree2",
                  trControl = fitControl,
                  tuneLength = tuneLength
)
time2 <- proc.time()-ptm
cat(time2)

resampspartyWV6 <- resamples(list(ctree1cleannonanswer = fitCtree1,
#                              rpart2 = fitRpart2,
#                              rpart1cleannonanswer = fitRpart4,
                             ctree2cleannonanswer = fitCtree2))
# 
save(resampspartyWV6, fitCtree2, file=file.path(datapath, "ctreetrain_WV6.Rdata"))

#######################
# stop parallel processing
#######################
stopCluster(cl)         
