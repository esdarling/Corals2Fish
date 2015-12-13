# ====================================
# = Import Graham coral + fish data  =
# ====================================
setwd("/Users/emilydarling/Dropbox/1-On the go/Graham_FD test/Data")         
dat <- read.csv("Graham_coralfish_25Sept2014.csv", header = TRUE, stringsAsFactors = TRUE) 
names(dat)    

unique(dat$Year)
unique(dat$Country)

unique(dat$Habitat)
unique(dat$Management)

hist(dat$Depth_m)
min(dat$Depth_m); max(dat$Depth_m)

with(dat, table(Country, Habitat))


#consider VIFs 
source("HighstatLib.R")  


##LIFE HISTORY
#two sets of analyses: coral cover / life histories 
Z5 <- dat[,c("Competitive","StressTolerant","Generalist","Weedy")]
corvif(Z5) 

Z5b <- dat[,c("total_cover","Competitive","StressTolerant","Generalist","Weedy")]
corvif(Z5b)

#Final set for life history models
Zall <- dat[,c("Complexity","no_genera","Rao",
               "CWMBranching","CWMMaxsize","CWMBrooding","CWMFecundity",
               "Competitive","StressTolerant","Generalist","Weedy")]  
head(Zall)
corvif(Zall)        



