################
# analyzing wave 6 (ie singapore) data
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
sourcefile <- "WV6.RData"
codebook <- "WV6_Codebook_v_2014_11_07.csv"
fieldinfo <- "WVS_6_valuerange.csv"
mainvar <- "WV6"
wvs6df <- load.WVS(sourcefile, codebook, fieldinfo, mainvar)