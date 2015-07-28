##################
# common script to load data for WVS6
##################

#load data
sourcefile <- "WV6.RData"
# codebook <- "WV6_Codebook_v_2014_11_07.csv"
codebook <- "WVS_6_valuerange.csv"  # shown to be same result as above, can simplify
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