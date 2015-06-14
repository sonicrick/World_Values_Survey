################
# analyzing wave 6 (ie singapore) data
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
sourcefile <- "WV6.RData"
codebook <- "WV6_Codebook_v_2014_11_07.csv"
fieldinfo <- "WVS_6_valuerange.csv"
mainvar <- "WV6"
happinessfield="V10"
countryfield="V2"
d <- load.WVS(sourcefile, codebook, fieldinfo, mainvar, happinessfield, countryfield)

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(d$Happiness, p=.8, list=FALSE)
dtrain <- d[trainIndex, ]
dtest <- d[-trainIndex, ]

# train setting: set in WVS_lib.R

##############
# enable parallel processing
##############
require(doSNOW)
cl <- registerDoSNOW(makeCluster(4, type = "SOCK"))

# train 1: basic rpart 1
cat("Rpart 1")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitRpart1 <- train(Happiness ~ ., data = dtrain,
                   method = "rpart",
                   trControl = fitControl
)

# train 2: basic rpart2
cat("Rpart 2")
set.seed(12345) # need to set same seed for all training to have same fold separation?
fitRpart2 <- train(Happiness ~ ., data = dtrain,
                   method = "rpart2",
                   trControl = fitControl
)

resampsWv6 <- resamples(list(rpart1 = fitRpart1,
                             rpart2 = fitRpart2))

#######################
# stop parallel processing
#######################
stopCluster(cl)         