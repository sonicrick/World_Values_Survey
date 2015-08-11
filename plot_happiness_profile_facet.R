################
# plotting profile of a category
################

source('~/GitHub/World_Values_Survey/WVS_lib.R')

#load data
# source('~/GitHub/World_Values_Survey/load_WV6_for_caret.R')
# require(plyr); require(dplyr)
# require(ggplot2)
# require(tidyr)
# 
# #### NOTE: data processed will be using dnon (i.e. where NA answers are merged -> se load_WV6_for_caret.R)
# 
# # manual tweaking of variables to streamline partitioning
# # drop "Year of birth" in favour of age
# dnon <- dnon[, names(dnon) != "V241"]
# 
# 
# # set Unhappiness using dnon 
# dnon$Unhappiness <- "Y"
# dnon[which(d$Happiness %in% c("Very happy", "Quite happy")), ]$Unhappiness <- "N"
# dnon$Unhappiness <- relevel(factor(dnon$Unhappiness), ref="Y")  # caret needs this as factor to train
# # dnon$Happiness <- NULL  # eliminate the field to avoid confusion
# 
# 
# # read field descriptions in codebook
# fieldsRange <- read.csv(file.path(datapath, "WVS_6_valuerange.csv"),
#                         stringsAsFactors=FALSE)
# #keep only fields in dataset
# fieldsRange <- fieldsRange[fieldsRange$VARIABLE %in% names(dnon), ]
# numScale <- fieldsRange$VARIABLE[fieldsRange$NonStandard!="Y"]

# # split training and test set
# set.seed(13579)
# trainIndex <- createDataPartition(dnon$Unhappiness, p=.8, list=FALSE)
# dnontrain <- dnon[trainIndex, ]
# dnontrain <- upSample(dnontrain, dnontrain$Unhappiness, list=T)[[1]]
# dnontest <- dnon[-trainIndex, ]

smooth.colour <- function (df, var) {
  dtot <- df %>% group_by_(.dots=var) %>%
    summarize(total=n())
  res <- resolution*nrow(dtot)
  dplot <- data.frame(x=spline(dtot, n=res)[[1]],
                      f=spline(dtot, n=res)[[2]],
                      variable=var)
  # variable=fieldsRange$LABEL[fieldsRange$VARIABLE == var])
  dplot <- dplot %>%
    mutate(i=x - lag(x))  # this is interval between each x. used for creating stacked plot later)
  dplot$i[is.na(dplot$i)] <- 0
  return(dplot)
}

loop.smooth.colour <- function(df, vars, grouping, groupChoice) {
  dsub <- df[df[[grouping]]==groupChoice, c(vars, grouping)]
  numIdx <- which(names(dsub) %in% numScale)
  dsub[, numIdx] <- sapply(dsub[, numIdx], function(x) as.numeric(as.character(x)))
  
  dplot <- do.call(rbind, lapply(vars, function(x) smooth.colour(dsub[, names(dsub)!=grouping], x)))
  dplot[[grouping]] <- groupChoice
  return(dplot)
}

grChoice <- c("Y", "N")


# varnames <- c("V204", "V203A")
varnames <- c("V204", "V203A", "V59", "V202", "V206", "V201", "V133", "V200", 
          "V193", "V56", "V55", "V137", "V208", "V97", "V192", "V232", 
          "V205", "V210", "V203", "V160", "V207", "V233", "V139", "V197", 
          "V140", "V98", "V71", "V141", "V23", "V99")
grp <- "Unhappiness"
resolution=5


dplot <- do.call(rbind, lapply(grChoice, function(x)
  loop.smooth.colour(dnon, varnames, grp, x)
  ))

# dsub <- dnon[dnon[[grouping]]==groupChoice, c(vars, grouping)]
# numIdx <- which(names(dsub) %in% numScale)
# dsub[, numIdx] <- sapply(dsub[, numIdx], function(x) as.numeric(as.character(x)))
# 
# dplot <- do.call(rbind, lapply(vars, function(x) smooth.colour(dsub[, names(dsub)!=grouping], x)))
# dplot[[grouping]] <- groupChoice

# location of peak to be marked with a bar
# dpeak <- dplot %>% group_by(variable) %>%
dpeak <- dplot %>% group_by_(.dots=list("variable", grp)) %>%  
  summarise(peak=max(f), rowpos=which(f==max(f)), 
            x=x[rowpos], loc=sum(i[1:rowpos])/sum(i))

# create the plot
g <- ggplot(data=dplot, aes(x=variable)) + scale_fill_gradient(low="#ffffe5", high="#7f0000") + coord_flip()
g1 <- g + geom_bar(aes(y=i, fill=f), stat="identity", position="fill") +
  scale_x_discrete(limits=rev(levels(dplot$variable))) +   # reverse scale so first variable is on top
  geom_errorbar(data=dpeak, aes(x=variable, y=loc, ymax=loc, ymin=loc),
                size=2, color="blue")
# NOTE : RESULT UNCHECKED, LOOKS LIKE FACET HEADER MAY BE REVERSED

# # experimental plotting as boxplot
# # shape into long form for all variables
# ds <- dsub[, names(dsub)!=grouping] %>% gather(Variable, Value)
# # tabulate for plotting  
# dsum <- as.data.frame(table(ds))
# 
# gnon <- ggplot(data=ds) + coord_flip()
# g2 <- gnon + geom_boxplot(aes(x=Variable, y=Value))
# g2 + scale_x_discrete(limits=rev(levels(ds$Variable)))
