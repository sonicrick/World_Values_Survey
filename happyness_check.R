require(dplyr)

setwd("~/GitHub/World_Values_Survey")
datapath <- "~/GitHub/World_Values_Survey/data"

load(file.path(datapath, "WV_long.RData"))
fields <- read.csv(file.path(datapath, "filtered WVS_Values Surveys Integrated Dictionary_TimeSeries_v_2014-04-25.csv"),
                   stringsAsFactors=FALSE)

varnames <- fields$VARIABLE
varnames <- varnames[varnames!=""]  # eliminate blank fields
map <- which(names(WVL) %in% varnames)
unmap <- which(!(names(WVL) %in% varnames))
notinWVL <- which(!(varnames %in% names(WVL)))

Sin <- WVL[WVL$S003==702, map]
Fin <- WVL[WVL$S003==246, map]
B <- WVL[, map] # master
B$S003 <- factor(B$S003)
levels(B$S003) <- c("Finland", "Singapore")
hap.level <- c("No answer", "Don't know", "Very happy", "Quite happy", "Not very happy", "Not at all happy")

####plot happiness (A008) facet Fin/Sin over year 
Bin <- B
Bin$A008 <- factor(Bin$A008)
levels(Bin$A008) <- hap.level
Bin$year <- substr(Bin$S025, 4, 7)
g <- ggplot(Bin) + geom_bar(aes(x=A008, fill=A008)) + facet_grid(S003 ~ year)


#### check correlation Happiness and Satisfaction with life (A170) and with Finance (C006)

# filter out those without data on Happiness
Bin <- Bin[!(Bin$A008 %in% c("No answer", "Don't know")), ]  # eliminate those with unknown/missing answer etc on happiness

# filter out those without data on satisfaction
Bin.S <- Bin[Bin$A170 > 0, ] # eliminate those with unknown/missing answer etc on satisfaction
s.life <- Bin.S %>% group_by(A008, A170, S003) %>% summarize(Count=n())
g.sl <- ggplot(s.life, aes(x=A170, y=A008)) + geom_point(aes(size=Count)) + facet_grid(S003 ~ .) +
  labs(x="Satisfaction in Life (Dissatisfied 1 - 10 Satisfied)", y="Happiness") + scale_x_continuous(breaks=1:10)

# filter out those without data on finance
Bin.F <- Bin[Bin$C006 > 0, ] # eliminate those with unknown/missing answer etc on satisfaction in finance
s.finance <- Bin.F %>% group_by(A008, C006, S003) %>% summarize(Count=n())
g.sf <- ggplot(s.finance, aes(x=C006, y=A008)) + geom_point(aes(size=Count)) + facet_grid(S003 ~ .) +
  labs(x="Satisfaction in Household Finance (Dissatisfied 1 - 10 Satisfied)", y="Happiness") + scale_x_continuous(breaks=1:10)
