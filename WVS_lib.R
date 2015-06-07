#################
# library of common functions for WVS project
#################

setwd("~/GitHub/World_Values_Survey")
datapath <- "~/GitHub/World_Values_Survey/data"

################
# load longitudinal data for WVS filtered for Finland and Singapore
# set up for Happiness as dependent variable
################
load.WVS.long.happy <- function() {
  load(file.path(datapath, "WV_long.RData"))
  fields <- read.csv(file.path(datapath, "filtered WVS_Values Surveys Integrated Dictionary_TimeSeries_v_2014-04-25.csv"),
                     stringsAsFactors=FALSE)
  
  varnames <- fields$VARIABLE
  varnames <- varnames[varnames!=""]  # eliminate blank fields
  map <- which(names(WVL) %in% varnames)
  
  Sin <- WVL[WVL$S003==702, map]
  Fin <- WVL[WVL$S003==246, map]
  Bin <- WVL[, map] # master
  Bin$S003 <- factor(Bin$S003)
  levels(Bin$S003) <- c("Finland", "Singapore")

  Bin$year <- substr(Bin$S025, 4, 7)

  # filter out those without data on Happiness
  Bin <- Bin[!(Bin$A008 %in% c("No answer", "Don't know")), ]  # eliminate those with unknown/missing answer etc on happiness
  hap.level <- c("No answer", "Don't know", "Very happy", "Quite happy", "Not very happy", "Not at all happy")
  Bin$A008 <- factor(Bin$A008)
  levels(Bin$A008) <- hap.level

  return(Bin)
}
