# ====================================
# = Import Graham coral + fish data  =
# ====================================
setwd("/Users/emilydarling/Dropbox/1-On the go/Graham_FD test/Data")         
dat <- read.csv("Graham_coralfish_25Sept2014.csv", header = TRUE, stringsAsFactors = TRUE) 
names(dat)   

table(with(dat, Country))

min(dat$FishBiomass)
max(dat$FishBiomass)

unique(dat$Year)
unique(dat$Country)

unique(dat$Habitat)
unique(dat$Management)

table(dat$Management)

hist(dat$Depth_m)
min(dat$Depth_m); max(dat$Depth_m)

with(dat, table(Country, Habitat))

#plot coral cover to structural complexity
names(dat)
head(dat)

dat.ggplot <- dat %>% 
  select(Complexity, Latitude,StressTolerant,Competitive,no_genera,Generalist,
         CWMMaxsize,Weedy,CWMBrooding,Depth_m,CWMFecundity,CWMBranching,total_cover) %>% 
  melt(id.vars = "Complexity")

dat.ggplot$variable <- recode(dat.ggplot$variable, 
                              "'total_cover' = 'Total cover';
                              'no_genera' = 'Genera richness';
                              'CWMMaxsize' = 'Colony size';
                              'CWMBrooding' ='Brooding';
                              'CWMFecundity' = 'Fecundity';
                              'CWMBranching' = 'Branching';
                              'Depth_m' = 'Depth'")

head(dat.ggplot)
levels(as.factor(dat.ggplot$variable))

dat.ggplot$variable <- factor(dat.ggplot$variable, levels = c("Latitude","StressTolerant","Competitive","Genera richness","Generalist",
                                                              "Colony size","Weedy","Brooding","Depth","Fecundity","Branching","Total cover"))

## FIG 2
ggplot(data = dat.ggplot, aes(x = value, y = Complexity)) + 
  geom_point(shape = 21, alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 3) + 
  stat_smooth(method = "loess", colour = "red", se = TRUE) +
  stat_smooth(method = "lm", colour = "blue", se = TRUE) + 
  theme_bw(base_size = 12) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank()) + 
  scale_x_continuous(expand = c(0.025,0.025)) + 
  scale_y_continuous(breaks = c(1,2,3,4), limits = c(0,4.5)) + 
  xlab("Value") + 
  ylab("Structural complexity")

getwd()
setwd("/Users/emilydarling/Documents/Work/GitHub/esdarling/Corals2Fish/plots")   
ggsave("complex multiplot_withdummy.pdf", height = 6.225, width = 6.5)



#consider VIFs 
setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish")  
source("HighstatLib.R")  


##LIFE HISTORY
head(dat)
#two sets of analyses: coral cover / life histories 
Z5 <- dat[,c("Competitive","StressTolerant","Generalist","Weedy")]
corvif(Z5) 

Z5b <- dat[,c("total_cover","Competitive","StressTolerant","Generalist","Weedy")]
corvif(Z5b)

#VIFS if coral cover and life histories included in model
Zall <- dat[,c("Complexity","no_genera","Rao",
               "total_cover",
               "CWMBranching","CWMMaxsize","CWMBrooding","CWMFecundity","CWMGrowthrate",
               "Competitive","StressTolerant","Generalist","Weedy")]  
head(Zall)
corvif(Zall)    

#Final set for life history models
Zfinal <- dat[,c("Complexity","no_genera","Rao",
               "CWMBranching","CWMMaxsize","CWMBrooding","CWMFecundity","CWMGrowthrate",
               "Competitive","StressTolerant","Generalist","Weedy")]  
head(Zfinal)
corvif(Zfinal)       

#Final set for life history models
Zcover <- dat[,c("Complexity","no_genera","Rao",
                 "CWMBranching","CWMMaxsize","CWMBrooding","CWMFecundity","CWMGrowthrate",
                 "total_cover")]  
head(Zcover)
corvif(Zcover)       


# Fish results plots
min(dat$Complexity); max(dat$Complexity)
#abundance
ggplot(data = dat, aes(x = Complexity, y = FishAbundance)) +
  geom_point(aes(fill = Habitat), shape = 21, size = 4, 
             colour = "darkgrey", alpha = 0.8) + 
  stat_smooth(method = "lm", se = TRUE) +
  stat_smooth(method = "loess", se = TRUE, colour = "red") +
  scale_fill_manual(values = c("grey10","grey70", "white")) +
  theme_bw(base_size = 20) +
  scale_x_continuous(limits = c(0.5,3.69), expand = c(0.01,0.01)) +
  xlab("Structural complexity") +
  ylab("Abundance") +
  theme(legend.key = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("abundance plot.pdf", height= 3, width = 6, units = "in")    

#biomass
dat$logFishBiomass <- log10(dat$FishBiomass)   
min(dat$logFishBiomass); max(dat$logFishBiomass)

ggplot(data = dat, aes(x = Complexity, y = logFishBiomass)) +
  geom_point(aes(fill = Management), shape = 21, size = 4, 
             colour = "darkgrey", alpha = 0.8) + 
  stat_smooth(method = "lm", se = TRUE) +
  stat_smooth(method = "loess", se = TRUE, colour = "red") +
  scale_fill_manual(values = c("white","grey10")) +
  theme_bw(base_size = 19) +
  scale_x_continuous(limits = c(0.5,3.69), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(1.5,4.5), breaks = c(2,3,4)) +
  xlab("Structural complexity") +
  ylab(expression(paste("log(Biomass, kg ", ha^-1,")"))) +
  theme(legend.key = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("biomass plot.pdf", height= 3, width = 6.8, units = "in")    

#diversity
ggplot(data = dat, aes(x = Complexity, y = FishDiversity)) +
  geom_point(aes(fill = Habitat), shape = 21, size = 4, 
             colour = "darkgrey", alpha = 0.8) + 
  stat_smooth(method = "lm", se = TRUE) +
  stat_smooth(method = "loess", se = TRUE, colour = "red") +
  scale_fill_manual(values = c("grey10","grey70", "white")) +
  theme_bw(base_size = 19) +
  scale_x_continuous(limits = c(0.5,3.69), expand = c(0.01,0.01)) +
  xlab("Structural complexity") +
  ylab("Species richness") +
  theme(legend.key = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("diversity plot.pdf", height= 3.2, width = 6, units = "in")   

## predictors of complexity models
names(dat)
dat_melt2 <- melt(dat[,c(4,8,12,10,33:37,29,24,31,27,9)], id.vars = 1:3, variable.name = "CoralResponse") 
head(dat_melt2) 
levels(dat_melt2$CoralResponse)

#add a dummy column blank in for reef zone/habitat
#recode facet labels
dat_melt2$CoralResponse <- recode(dat_melt2$CoralResponse, 
                                  "'no_genera' = 'Genera richness';
'CWMMaxsize' = 'Colony size'; 'CWMBrooding' = 'Brooding'; 'Depth_m' = 'Depth';
'CWMFecundity' = 'Fecundity'; 'CWMBranching' = 'Branching'")

dat_melt2$CoralResponse <- factor(dat_melt2$CoralResponse, 
                                  levels = c("Latitude","StressTolerant","Competitive",
                                            "Genera richness","Generalist",
                                            "Colony size","Weedy",
                                            "Brooding","Depth",
                                            "Fecundity","Branching"))
levels(dat_melt2$CoralResponse)

ggplot(data = dat_melt2, aes(x = value, y = Complexity)) +
  geom_point(shape = 21, alpha = 0.45, size = 3,
             colour = "black", fill = "grey90") + 
  stat_smooth(method = "lm", se = TRUE) +
  stat_smooth(method = "loess", se = TRUE, colour = "red") +
  scale_y_continuous(limits = c(0.5,4.5)) +
  facet_wrap(~ CoralResponse, scales = "free", ncol =3) +
  theme_bw(base_size = 19) +
  ylab("Structural complexity") +
  theme(legend.key = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())      
setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("complex multiplot_withdummy.pdf", height= 10, width = 10, units = "in") 

#reef zone/habitat
head(dat)
names(dat)

levels(dat$Habitat)
dat$Habitat <- factor(dat$Habitat, levels = c("Slope","Crest","Flat"))

ggplot(data = dat, aes(x = Habitat, y = Complexity)) +
  geom_boxplot() + 
  theme_bw(base_size = 19) +
  scale_y_continuous(limits = c(0.5,4.5)) +
  theme(legend.key = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 
setwd("/Users/emilydarling/Documents/Work/GitHub/Corals2Fish/1_plots")  
ggsave("habitat_for complex multiplot.pdf", height= 2.5, width = 3.3, units = "in") 
