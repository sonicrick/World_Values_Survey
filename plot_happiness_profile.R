################
# plotting profile of a category
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')
require(plyr); require(dplyr)
require(ggplot2)

#### NOTE: data processed will be using dnon (i.e. where NA answers are merged -> se load_WV6_for_caret.R)

# manual tweaking of variables to streamline partitioning
# drop "Year of birth" in favour of age
dnon <- dnon[, names(dnon) != "V241"]


# set Unhappiness using dnon 
dnon$Unhappiness <- "Y"
dnon[which(d$Happiness %in% c("Very happy", "Quite happy")), ]$Unhappiness <- "N"
dnon$Unhappiness <- relevel(factor(dnon$Unhappiness), ref="Y")  # caret needs this as factor to train
# dnon$Happiness <- NULL  # eliminate the field to avoid confusion

# # split training and test set
# set.seed(13579)
# trainIndex <- createDataPartition(dnon$Unhappiness, p=.8, list=FALSE)
# dnontrain <- dnon[trainIndex, ]
# dnontrain <- upSample(dnontrain, dnontrain$Unhappiness, list=T)[[1]]
# dnontest <- dnon[-trainIndex, ]

vars <- c("V204")
grouping <- "Unhappiness"
groupChoice <- "Y"
dplot <- dnon[dnon[[grouping]]==groupChoice, c(vars, grouping)]

g <- ggplot(data=dplot)
g + geom_
