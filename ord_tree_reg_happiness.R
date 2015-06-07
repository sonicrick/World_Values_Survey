#################
# perform tree regression on longitudinal data
#################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

require(caret)
require(rcart)

# load longitudinal data
d <- load.WVS.long.happy()

# get field possible value and fields to ignore
fieldsRange <- read.csv(file.path(datapath, "WVS_L_filtered_valuerange.csv"),
                        stringsAsFactors = FALSE)

# drop fields to ignore
d <- d[, !(names(d) %in% fieldsRange$VARIABLE[fieldsRange$IGNORE=="Y"])]

# rename for convenience
names(d)[names(d)=="A008"] <- "Happiness"
names(d)[names(d)=="S003"] <- "Country"

# split training and test set
set.seed(13579)
trainIndex <- createDataPartition(d$Happiness, p=.8, list=FALSE)
dtrain <- d[trainIndex, ]
dtest <- d[-trainIndex, ]

# train!
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
hapFit <- train(Happiness ~ ., data = dtrain,
                method = "rcart",
                trControl = fitControl
                )