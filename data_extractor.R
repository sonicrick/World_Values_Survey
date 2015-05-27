#######################################################
#### To reduce data size to only relevant ones (i.e. Singapore and/or Finland)
#######################################################

setwd("~/GitHub/World_Values_Survey")
datapath <- "~/GitHub/World_Values_Survey/data"

## notes: V2 Country codes for Singapore == 702, for Finland == 246
ct <- c(702, 246)

# Wave 6
load(file.path(datapath, "WV6_Data_r_v_2015_04_18.rdata"))
WV6 <- WV6_Data_v_2015_04_18
WV6 <- WV6[WV6$V2A %in% ct, ]
rm(WV6_Data_v_2015_04_18)

# Wave 6 integrated results
load(file.path(datapath, "WV6_IntegratedData_rdata_v_2014_06_17.rdata"))
WV6i <- WV6_IntegratedData_spss_v_2014_06_17
WV6i <- WV6i[WV6i$S003A %in% ct, ]
rm(WV6_IntegratedData_spss_v_2014_06_17)

# Wave 5
load(file.path(datapath, "WV5_Data_R_v_2015_04_18.rdata"))
WV5 <- WV5_Data_r_v_2015_04_18
WV5 <- WV5[WV5$V2A %in% ct, ]
rm(WV5_Data_r_v_2015_04_18)

# Wave 5 integrated results
load(file.path(datapath, "WV5_IntegratedData_rdata_v_2014_06_17.rdata"))
WV5i <- WV5_IntegratedData_spss_v_2014_06_17
WV5i <- WV5i[WV5i$S003A %in% ct, ]
rm(WV5_IntegratedData_spss_v_2014_06_17)

# longitudinal results
load(file.path(datapath, "WVS_Longitudinal_1981_2014_R_v2015_04_18.rdata"))
WVL <- WVS_Longitudinal_1981_2014_R_v2015_04_18
WVL <- WVL[WVL$S003A %in% ct, ]
rm(WVS_Longitudinal_1981_2014_R_v2015_04_18)

save(WV5, WV5i, file=file.path(datapath, "WV5.RData"))
save(WV6, WV6i, file=file.path(datapath, "WV6.RData"))
save(WVL, file=file.path(datapath, "WV_long.RData"))
