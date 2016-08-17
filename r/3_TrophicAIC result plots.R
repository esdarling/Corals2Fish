library(car)
library(stats)
library(gdata)
library(foreign)
#library(glmulti)    
library(nlme)  
library(MuMIn)  
library(arm) 

library(dplyr)
library(reshape)
library(cowplot)

#install.packages(("cowplot"))


# ====================================
# = Import new trophic group info  =
# ====================================
setwd("/Users/emilydarling/Documents/Work/GitHub/esdarling/Corals2Fish/data")   
dat <- read.csv("Emily by functional group.csv", header = TRUE, stringsAsFactors = TRUE) 
head(dat)
nrow(dat) 
names(dat)

#remove extra columns
dat <- dat[,-c(45:110)]
names(dat)

#trophic group histograms
head(dat)
dat2 <- dat %>% 
  select(Site,Corallivore.biom,Herbivore.biom,Invertivore.biom,Mixed.diet.biom,
         Piscivore.biom,Planktivore.biom) %>% 
  melt(id.vars = 1)

head(dat2)
names(dat2)[3] <- "biomass"
unique(dat2$variable)

ggplot(data = dat2, aes(x = log10(biomass+1))) + 
  geom_histogram(aes(fill = variable), bins = 20) + 
  facet_wrap(~variable, ncol = 3, scales = "free") +
  theme_bw(base_size = 16) +
  scale_y_continuous(expand = c(0.05,0.05)) +
  theme(legend.position = "none")

#setwd("/Users/emilydarling/Documents/Work/GitHub/esdarling/Corals2Fish/trophic_outputs")  
#ggsave("trophic group histograms.pdf", width = 6, height = 4)

# ====================================
# = Plot model coefficients   
# ====================================
setwd("/Users/emilydarling/Documents/Work/GitHub/esdarling/Corals2Fish/trophic_outputs")   
avg <- read.csv("trophic model avgs.csv", header = TRUE, stringsAsFactors = TRUE) 
head(avg)
nrow(avg) 
names(avg)

#remove intercept from graphs
avg <- subset(avg, variable != "Intercept")

#rename variable
unique(avg$variable)
avg$variable <- recode(avg$variable, 
                       "'no_genera' = 'Genera richness';
                       'CWMMaxsize' = 'Colony size';
                       'CWMBrooding' = 'Brooding';
                       'Depth_m' = 'Depth';
                       'CWMFecundity' = 'Fecundity';
                       'CWMBranching' = 'Branching';
                       'HabitatFlat' = 'Habitat, flat';
                       'HabitatSlope' = 'Habitat, slope';
                       'ManagementMarine reserve' = 'Marine reserve'")

#create colours from ggplot2
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

n = 6
cols = gg_color_hue(n)


#create list of trophic groups in data to loop over
trophic_list <- unique(avg$trophic)
trophic_list

#corallivore
ggplot(subset(avg, trophic == trophic_list[1]),
       aes(x = reorder(variable,Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = lower.95ci , ymax = upper.95ci), 
                  colour = cols[1], size = 0.75) + 
  geom_hline(aes(yintercept = 0), colour = "grey") +
  coord_flip() +
  theme_bw(base_size=16) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())
    
ggsave("myplotCorallivore.pdf", width = 3.5, height = 3)

#herbivore
ggplot(subset(avg, trophic == trophic_list[2]),
       aes(x = reorder(variable,Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = lower.95ci , ymax = upper.95ci), 
                  colour = cols[2], size = 0.75) + 
  geom_hline(aes(yintercept = 0), colour = "grey") +
  coord_flip() +
  theme_bw(base_size=16) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

ggsave("myplotHerbivore.pdf", width = 3.8, height = 3)

#invertivore
ggplot(subset(avg, trophic == trophic_list[3]),
       aes(x = reorder(variable,Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = lower.95ci , ymax = upper.95ci), 
                  colour = cols[3], size = 0.75) + 
  geom_hline(aes(yintercept = 0), colour = "grey") +
  coord_flip() +
  theme_bw(base_size=16) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

ggsave("myplotInvertivore.pdf", width = 3.8, height = 3)

#mixed-diet
ggplot(subset(avg, trophic == trophic_list[4]),
       aes(x = reorder(variable,Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = lower.95ci , ymax = upper.95ci), 
                  colour = cols[4], size = 0.75) + 
  geom_hline(aes(yintercept = 0), colour = "grey") +
  coord_flip() +
  theme_bw(base_size=16) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

ggsave("myplotMixed.diet.pdf", width = 3.8, height = 3)

#piscivore
ggplot(subset(avg, trophic == trophic_list[5]),
       aes(x = reorder(variable,Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = lower.95ci , ymax = upper.95ci), 
                  colour = cols[5], size = 0.75) + 
  geom_hline(aes(yintercept = 0), colour = "grey") +
  coord_flip() +
  theme_bw(base_size=16) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

ggsave("myplotPiscivore.pdf", width = 3.92, height = 3)

#planktivore
ggplot(subset(avg, trophic == trophic_list[6]),
       aes(x = reorder(variable,Estimate), y = Estimate)) + 
  geom_pointrange(aes(ymin = lower.95ci , ymax = upper.95ci), 
                  colour = cols[6], size = 0.75) + 
  geom_hline(aes(yintercept = 0), colour = "grey") +
  coord_flip() +
  theme_bw(base_size=16) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

ggsave("myplotPlanktivore.pdf", width = 3.92, height = 3.1)

