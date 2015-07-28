################
# analyzing wave 6 (ie Singapore) data
# using only 23 variables reconciled Han Ei vs Automatic feature elimination
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

############### prepared dataset

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')

# load 23 variables reconciling HanEi and autoforecast
load(file.path(datapath, "reconcile_HE_auto.RData"))

# create smaller set with only the 23 and Happiness level
d23 <- d[, names(d) %in% c("Happiness", as.character(reconcile$var))]

# merge various non-answers (Unknown, NA, etc) into one factor level
d23 <- merge.nonanswers(d23)


# set Unhappiness
d23$Unhappiness <- T
d23$Unhappiness[d23$Happiness %in% c("Very happy", "Quite happy")] <- F
d23$Unhappiness <- factor(d23$Unhappiness)  # caret needs this as factor to train
# d23$Happiness <- NULL  # eliminate the field to avoid confusion
wt <- table(d23$Unhappiness)/nrow(d23)  # parameter for class weightage in training below


# split training and test set, using same trainIndex in load_WV6_for_caret.R
d23train <- d23[trainIndex, ]
d23test <- d23[-trainIndex, ]

# multiply unhappy records to eliminate unbalanced in training data
scaleFactor <- round(wt[1]/wt[2])
dtrue <- d23train[d23train$Unhappiness=="TRUE", ]
d23repl <- NULL
for (x in 1:scaleFactor) d23repl <- rbind(d23repl, dtrue)
# add back happiness
d23repl <- rbind(d23repl, d23train[d23train$Unhappiness=="FALSE", ])

############### ENDOF prepared dataset

# train setting: set in WVS_lib.R
tuneLength <- 5

# enable parallel processing
require(doSNOW)
cl <- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

############### Start training for 4-class classifier

# train 1: basic random forest
cat("Random forest")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fit23Rf <- train(Happiness ~ ., data = d23train[, names(d23train) != "Unhappiness"],
               method = "rf",
               trControl = fitControl,
               tuneLength = tuneLength
)
time1 <- proc.time()-ptm
cat(time1)

# train 2: train with oversampling
cat("oversampling")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fit23oversamp <- train(Happiness ~ ., data = d23repl[, names(d23repl) != "Unhappiness"],
                 method = "rf",
                 trControl = fitControl,
                 tuneLength = tuneLength
)
time2 <- proc.time()-ptm
cat(time2)


resamps23_RfWV6 <- resamples(list(rf23 = fit23Rf,
                              rf23oversamp = fit23oversamp))

accuracy_chk <- function(fitmodel, testfile, targetfield) {
  pred <- predict(fitmodel, testfile)
  chkdf <- data.frame(predicted=pred, actual=testfile[[targetfield]])
  crosstab <- with(chkdf, table(predicted, actual))
  chksum <- chkdf %>% group_by(predicted, actual) %>% summarize(count=n())
  accur <- with(chkdfsum, sum(count[predicted==actual]))/nrow(testfile)
  return(list(chkdf=chkdf,
              crosstab=crosstab,
              chksum=chksum,
              accur=accur))
}

# own accuracy check
acc23RfHap <- accuracy_chk(fit23Rf, d23test[, names(d23test) != "Unhappiness"], "Happiness")
# # own accuracy check
# pred23Rf <- predict(fit23Rf, d23test[, names(d23test) != "Unhappiness"])
# chkdf <- data.frame(predicted=pred23Rf, actual=d23test$Happiness)
# crosstab <- with(chkdf, table(predicted, actual))
# chkdfsum <- chkdf %>% group_by(predicted, actual) %>% summarize(count=n())
# accurRf <- with(chkdfsum, sum(count[predicted==actual]))/nrow(d23test)

acc23RfHapOver <- accuracy_chk(fit23oversamp, d23test[, names(d23test) != "Unhappiness"], "Happiness")
# pred23Over <- predict(fit23oversamp, d23test[, names(d23test) != "Unhappiness"])
# chkdfOver <- data.frame(predicted=pred23Over, actual=d23test$Happiness)
# crosstabOver <- with(chkdfOver, table(predicted, actual))
# chkdfsumOver <- chkdfOver %>% group_by(predicted, actual) %>% summarize(count=n())
# accurOver <- with(chkdfsumOver, sum(count[predicted==actual]))/nrow(d23test)

############### ENDOF Start training for 4-class classifier


############### Start training forUnhappiness
cat("Unhappiness rf")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fit23RfUnhap <- train(Unhappiness ~ ., data = d23train[, names(d23train) != "Happiness"],
                            method = "rf",
                            trControl = fitControl,
                            tuneLength = tuneLength
)
time3 <- proc.time()-ptm
cat(time3)

cat("Unhappiness oversampled rf")
set.seed(12345) # need to set same seed for all training to have same fold separation?
ptm <- proc.time()
fit23RfUnhapOver <- train(Unhappiness ~ ., data = d23repl[, names(d23repl) != "Happiness"],
                      method = "rf",
                      trControl = fitControl,
                      tuneLength = tuneLength
)
time4 <- proc.time()-ptm
cat(time4)

resamps23_Rf_unh_WV6 <- resamples(list(rf23Unhap = fit23RfUnhap,
                                  rf23UnhapOver = fit23RfUnhapOver))


# own accuracy check
acc23RfUnhap <- accuracy_chk(fit23RfUnhap, d23test[, names(d23test) != "Happiness"], "Unhappiness")
acc23RfUnhapOver <- accuracy_chk(fit23RfUnhapOver, d23test[, names(d23test) != "Happiness"], "Unhappiness")

#######################
# stop parallel processing
#######################
stopCluster(cl)   
