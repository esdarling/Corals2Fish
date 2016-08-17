.libPaths()

#update libraries..
library(car)
library(stats)
library(gdata)
library(foreign)
#library(glmulti)    
library(nlme)  
library(MuMIn)  
library(arm) 

library(dplyr)
library(reshape2)


# ====================================
# = Import Graham coral + fish data  =
# ====================================
getwd()

dat <- read.csv("Emily by functional group.csv", header = TRUE, stringsAsFactors = TRUE) 
head(dat)
nrow(dat) 
names(dat)

#remove extra columns
dat <- dat[,-c(45:110)]
names(dat)

#trophic group histograms
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

ggsave("trophic group histograms.pdf", width = 6, height = 4)

# =====================================
# = Standardize predictors for MuMIn  =
# =====================================
?rescale
#Standardizing Regression Predictors by Centering and Dividing by 2 SD (package: arm)
dat$Depth_m.rescale <- rescale(dat$Depth_m, "full")
dat$Complexity.rescale <- rescale(dat$Complexity, "full")  
dat$no_genera.rescale <- rescale(dat$no_genera, "full")  
dat$Rao.rescale <- rescale(dat$Rao, "full")  
dat$CWMBranching.rescale <- rescale(dat$CWMBranching, "full")  
dat$CWMMaxsize.rescale <- rescale(dat$CWMMaxsize, "full")  
dat$CWMBrooding.rescale <- rescale(dat$CWMBrooding, "full")  
dat$CWMFecundity.rescale <- rescale(dat$CWMFecundity, "full")  
dat$Competitive.rescale <- rescale(dat$Competitive, "full")  
dat$StressTolerant.rescale <- rescale(dat$StressTolerant, "full")  
dat$Generalist.rescale <- rescale(dat$Generalist, "full")  
dat$Weedy.rescale <- rescale(dat$Weedy, "full")               

hist(dat$Depth_m.rescale)               

#Reorder habitat to compare to flat
dat$Habitat <- factor(dat$Habitat, levels = c("Flat","Crest","Slope"))
levels(dat$Habitat)     


# ========================================== 
# TROPHIC GROUPS - log Fish Biomass  =
# ==========================================  
# ===================
# 1. Corallivores  =
# ===================
names(dat)

## Fish Biomass - log transform instead of using gamma, fit with glm  
## log10 + 1 for 0 kg/ha in some trophic groups
dat$logCorallivore.biom <- log10(dat$Corallivore.biom + 1)
hist(dat$Corallivore.biom)     
hist(dat$logCorallivore.biom)     

## Check variance structure
#VarIdent
vf1 <- varIdent(form = ~1 | FishMethod)   
vf2 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management))    
vf3 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management), 
               varPower(form =~ Complexity))   


Biom0 <- gls(logCorallivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, data = dat)    

Biom1 <- lme(logCorallivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, 
             random = ~1 | Country, data = dat, na.action = na.fail, method = "REML")   

Biom2 <- lme(logCorallivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +	
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,
             random = ~1 | Country, weights = vf2, 
             data = dat, na.action = na.fail, method = "REML")   

Biom3 <- lme(logCorallivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,  
             random = ~1 | Country, weights = vf3, 
             data = dat, na.action = na.fail, method = "REML")  

AIC(Biom0, Biom1, Biom2, Biom3)    
#Random and varIdent are better
#varPower complexity better for corallivore biomass	    	

######################
## Dredge and Model Average  
CorallivoreBiom_model.set <- dredge(Biom3, m.max = 5) 

CorallivoreBiom_models <- data.frame(CorallivoreBiom_model.set)   
names(CorallivoreBiom_models)[1] <- "Intercept" 
head(CorallivoreBiom_models)
#write all models
write.csv(CorallivoreBiom_models, "CorallivoreBiom_all models.csv") 

CorallivoreBiom_top.models <- get.models(CorallivoreBiom_model.set, subset = delta < 4)   
CorallivoreBiom_top.models

CorallivoreBiom_top.avg <- model.avg(CorallivoreBiom_top.models )
CorallivoreBiom_top.avg

summary(CorallivoreBiom_top.avg)

CorallivoreBiom_top.avg$msTable
CorallivoreBiom_top.avg$coefficients
CorallivoreBiom_top.avg$coefArray
CorallivoreBiom_top.avg$formula

confint_95 <- as.data.frame(confint(CorallivoreBiom_top.avg, full = TRUE, level = 0.95))
confint_95$variable <- row.names(ci)

corall.avg <- bind_cols(confint_95, as.data.frame(coefTable(CorallivoreBiom_top.avg, full = TRUE)))
corall.avg
write.csv(corall.avg, "CorallivoreBiom_avg coefs.csv", row.names = FALSE)


# ===================
# 2. Herbivores  =
# ===================
names(dat)

## Fish Biomass - log transform instead of using gamma, fit with glm  
## log10 + 1 for 0 kg/ha in some trophic groups
dat$logHerbivore.biom <- log10(dat$Herbivore.biom + 1)
hist(dat$Herbivore.biom)     
hist(dat$logHerbivore.biom)     

## Check variance structure
#VarIdent
vf1 <- varIdent(form = ~1 | FishMethod)   
vf2 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management))    
vf3 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management), 
               varPower(form =~ Complexity))   


Biom0 <- gls(logHerbivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, data = dat)    

Biom1 <- lme(logHerbivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, 
             random = ~1 | Country, data = dat, na.action = na.fail, method = "REML")   

Biom2 <- lme(logHerbivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +	
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,
             random = ~1 | Country, weights = vf2, 
             data = dat, na.action = na.fail, method = "REML")   

Biom3 <- lme(logHerbivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,  
             random = ~1 | Country, weights = vf3, 
             data = dat, na.action = na.fail, method = "REML")  

AIC(Biom0, Biom1, Biom2, Biom3)    
#Random and varIdent are better
#varPower complexity better for Herbivore biomass	    	

######################
## Dredge and Model Average  
HerbivoreBiom_model.set <- dredge(Biom3, m.max = 5) 

HerbivoreBiom_models <- data.frame(HerbivoreBiom_model.set)   
names(HerbivoreBiom_models)[1] <- "Intercept" 
head(HerbivoreBiom_models)
#write all models
write.csv(HerbivoreBiom_models, "HerbivoreBiom_all models.csv") 

HerbivoreBiom_top.models <- get.models(HerbivoreBiom_model.set, subset = delta < 4)   
HerbivoreBiom_top.models

HerbivoreBiom_top.avg <- model.avg(HerbivoreBiom_top.models )
HerbivoreBiom_top.avg

summary(HerbivoreBiom_top.avg)

HerbivoreBiom_top.avg$msTable
HerbivoreBiom_top.avg$coefficients
HerbivoreBiom_top.avg$coefArray
HerbivoreBiom_top.avg$formula

confint_95 <- as.data.frame(confint(HerbivoreBiom_top.avg, full = TRUE, level = 0.95))
confint_95$variable <- row.names(confint_95)

herb.avg <- bind_cols(confint_95, as.data.frame(coefTable(HerbivoreBiom_top.avg, full = TRUE)))
herb.avg
write.csv(herb.avg, "HerbivoreBiom_avg coefs.csv", row.names = FALSE)

# ===================
# 3. Mixed Diet  =
# ===================
names(dat)

## Fish Biomass - log transform instead of using gamma, fit with glm  
## log10 + 1 for 0 kg/ha in some trophic groups
dat$logMixed.diet.biom <- log10(dat$Mixed.diet.biom + 1)
hist(dat$Mixed.diet.biom)     
hist(dat$logMixed.diet.biom)     

## Check variance structure
#VarIdent
vf1 <- varIdent(form = ~1 | FishMethod)   
vf2 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management))    
vf3 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management), 
               varPower(form =~ Complexity))   


Biom0 <- gls(logMixed.diet.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, data = dat)    

Biom1 <- lme(logMixed.diet.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, 
             random = ~1 | Country, data = dat, na.action = na.fail, method = "REML")   

Biom2 <- lme(logMixed.diet.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +	
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,
             random = ~1 | Country, weights = vf2, 
             data = dat, na.action = na.fail, method = "REML")   

Biom3 <- lme(logMixed.diet.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,  
             random = ~1 | Country, weights = vf3, 
             data = dat, na.action = na.fail, method = "REML")  

AIC(Biom0, Biom1, Biom2, Biom3)    
#Random and varIdent are better
#varPower complexity better for Mixed.diet biomass	    	

######################
## Dredge and Model Average  
Mixed.dietBiom_model.set <- dredge(Biom3, m.max = 5) 

Mixed.dietBiom_models <- data.frame(Mixed.dietBiom_model.set)   
names(Mixed.dietBiom_models)[1] <- "Intercept" 
head(Mixed.dietBiom_models)
#write all models
write.csv(Mixed.dietBiom_models, "Mixed.dietBiom_all models.csv") 

Mixed.dietBiom_top.models <- get.models(Mixed.dietBiom_model.set, subset = delta < 4)   
Mixed.dietBiom_top.models

Mixed.dietBiom_top.avg <- model.avg(Mixed.dietBiom_top.models )
Mixed.dietBiom_top.avg

summary(Mixed.dietBiom_top.avg)

Mixed.dietBiom_top.avg$msTable
Mixed.dietBiom_top.avg$coefficients
Mixed.dietBiom_top.avg$coefArray
Mixed.dietBiom_top.avg$formula

confint_95 <- as.data.frame(confint(Mixed.dietBiom_top.avg, full = TRUE, level = 0.95))
confint_95$variable <- row.names(confint_95)

mixed.avg <- bind_cols(confint_95, as.data.frame(coefTable(Mixed.dietBiom_top.avg, full = TRUE)))
mixed.avg
write.csv(mixed.avg, "Mixed.dietBiom_avg coefs.csv", row.names = FALSE)

# ===================
# 4. Piscivore  =
# ===================
names(dat)

## Fish Biomass - log transform instead of using gamma, fit with glm  
## log10 + 1 for 0 kg/ha in some trophic groups
dat$logPiscivore.biom <- log10(dat$Piscivore.biom + 1)
hist(dat$Piscivore.biom)     
hist(dat$logPiscivore.biom)     

## Check variance structure
#VarIdent
vf1 <- varIdent(form = ~1 | FishMethod)   
vf2 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management))    
vf3 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management), 
               varPower(form =~ Complexity))   


Biom0 <- gls(logPiscivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, data = dat)    

Biom1 <- lme(logPiscivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, 
             random = ~1 | Country, data = dat, na.action = na.fail, method = "REML")   

Biom2 <- lme(logPiscivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +	
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,
             random = ~1 | Country, weights = vf2, 
             data = dat, na.action = na.fail, method = "REML")   

Biom3 <- lme(logPiscivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,  
             random = ~1 | Country, weights = vf3, 
             data = dat, na.action = na.fail, method = "REML")  

AIC(Biom0, Biom1, Biom2, Biom3)    
#Random and varIdent are better
#varPower complexity better for Piscivore biomass	    	

######################
## Dredge and Model Average  
PiscivoreBiom_model.set <- dredge(Biom3, m.max = 5) 

PiscivoreBiom_models <- data.frame(PiscivoreBiom_model.set)   
names(PiscivoreBiom_models)[1] <- "Intercept" 
head(PiscivoreBiom_models)
#write all models
write.csv(PiscivoreBiom_models, "PiscivoreBiom_all models.csv") 

PiscivoreBiom_top.models <- get.models(PiscivoreBiom_model.set, subset = delta < 4)   
PiscivoreBiom_top.models

PiscivoreBiom_top.avg <- model.avg(PiscivoreBiom_top.models )
PiscivoreBiom_top.avg

summary(PiscivoreBiom_top.avg)

PiscivoreBiom_top.avg$msTable
PiscivoreBiom_top.avg$coefficients
PiscivoreBiom_top.avg$coefArray
PiscivoreBiom_top.avg$formula

confint_95 <- as.data.frame(confint(PiscivoreBiom_top.avg, full = TRUE, level = 0.95))
confint_95$variable <- row.names(confint_95)

pisc.avg <- bind_cols(confint_95, as.data.frame(coefTable(PiscivoreBiom_top.avg, full = TRUE)))
pisc.avg
write.csv(pisc.avg, "PiscivoreBiom_avg coefs.csv", row.names = FALSE)

# ===================
# 5. Planktivore  =
# ===================
names(dat)

## Fish Biomass - log transform instead of using gamma, fit with glm  
## log10 + 1 for 0 kg/ha in some trophic groups
dat$logPlanktivore.biom <- log10(dat$Planktivore.biom + 1)
hist(dat$Planktivore.biom)     
hist(dat$logPlanktivore.biom)     

## Check variance structure
#VarIdent
vf1 <- varIdent(form = ~1 | FishMethod)   
vf2 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management))    
vf3 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management), 
               varPower(form =~ Complexity))   


Biom0 <- gls(logPlanktivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, data = dat)    

Biom1 <- lme(logPlanktivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, 
             random = ~1 | Country, data = dat, na.action = na.fail, method = "REML")   

Biom2 <- lme(logPlanktivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +	
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,
             random = ~1 | Country, weights = vf2, 
             data = dat, na.action = na.fail, method = "REML")   

Biom3 <- lme(logPlanktivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,  
             random = ~1 | Country, weights = vf3, 
             data = dat, na.action = na.fail, method = "REML")  

AIC(Biom0, Biom1, Biom2, Biom3)    
#Random and varIdent are better
#varPower complexity better for Planktivore biomass	    	

######################
## Dredge and Model Average  
PlanktivoreBiom_model.set <- dredge(Biom3, m.max = 5) 

PlanktivoreBiom_models <- data.frame(PlanktivoreBiom_model.set)   
names(PlanktivoreBiom_models)[1] <- "Intercept" 
head(PlanktivoreBiom_models)
#write all models
write.csv(PlanktivoreBiom_models, "PlanktivoreBiom_all models.csv") 

PlanktivoreBiom_top.models <- get.models(PlanktivoreBiom_model.set, subset = delta < 4)   
PlanktivoreBiom_top.models

PlanktivoreBiom_top.avg <- model.avg(PlanktivoreBiom_top.models )
PlanktivoreBiom_top.avg

summary(PlanktivoreBiom_top.avg)

PlanktivoreBiom_top.avg$msTable
PlanktivoreBiom_top.avg$coefficients
PlanktivoreBiom_top.avg$coefArray
PlanktivoreBiom_top.avg$formula

confint_95 <- as.data.frame(confint(PlanktivoreBiom_top.avg, full = TRUE, level = 0.95))
confint_95$variable <- row.names(confint_95)

plank.avg <- bind_cols(confint_95, as.data.frame(coefTable(PlanktivoreBiom_top.avg, full = TRUE)))
plank.avg
write.csv(plank.avg, "PlanktivoreBiom_avg coefs.csv", row.names = FALSE)

# ===================
# 6. Invertivore  =
# ===================
names(dat)

## Fish Biomass - log transform instead of using gamma, fit with glm  
## log10 + 1 for 0 kg/ha in some trophic groups
dat$logInvertivore.biom <- log10(dat$Invertivore.biom + 1)
hist(dat$Invertivore.biom)     
hist(dat$logInvertivore.biom)     

## Check variance structure
#VarIdent
vf1 <- varIdent(form = ~1 | FishMethod)   
vf2 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management))    
vf3 <- varComb(varIdent(form = ~1 | FishMethod), varIdent(form = ~1 | Management), 
               varPower(form =~ Complexity))   


Biom0 <- gls(logInvertivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, data = dat)    

Biom1 <- lme(logInvertivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale, 
             random = ~1 | Country, data = dat, na.action = na.fail, method = "REML")   

Biom2 <- lme(logInvertivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +	
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,
             random = ~1 | Country, weights = vf2, 
             data = dat, na.action = na.fail, method = "REML")   

Biom3 <- lme(logInvertivore.biom ~ Habitat + Management + Depth_m.rescale + 
               Complexity.rescale + no_genera.rescale + Rao.rescale +
               CWMBranching.rescale + CWMMaxsize.rescale + CWMBrooding.rescale + 
               CWMFecundity.rescale + Competitive.rescale + StressTolerant.rescale +
               Generalist.rescale + Weedy.rescale,  
             random = ~1 | Country, weights = vf3, 
             data = dat, na.action = na.fail, method = "REML")  

AIC(Biom0, Biom1, Biom2, Biom3)    
#Random and varIdent are better
#varPower complexity better for Invertivore biomass	    	

######################
## Dredge and Model Average  
InvertivoreBiom_model.set <- dredge(Biom3, m.max = 5) 

InvertivoreBiom_models <- data.frame(InvertivoreBiom_model.set)   
names(InvertivoreBiom_models)[1] <- "Intercept" 
head(InvertivoreBiom_models)
#write all models
write.csv(InvertivoreBiom_models, "InvertivoreBiom_all models.csv") 

InvertivoreBiom_top.models <- get.models(InvertivoreBiom_model.set, subset = delta < 4)   
InvertivoreBiom_top.models

InvertivoreBiom_top.avg <- model.avg(InvertivoreBiom_top.models )
InvertivoreBiom_top.avg

summary(InvertivoreBiom_top.avg)

InvertivoreBiom_top.avg$msTable
InvertivoreBiom_top.avg$coefficients
InvertivoreBiom_top.avg$coefArray
InvertivoreBiom_top.avg$formula

confint_95 <- as.data.frame(confint(InvertivoreBiom_top.avg, full = TRUE, level = 0.95))
confint_95$variable <- row.names(confint_95)

invert.avg <- bind_cols(confint_95, as.data.frame(coefTable(InvertivoreBiom_top.avg, full = TRUE)))
invert.avg
write.csv(invert.avg, "InvertivoreBiom_avg coefs.csv", row.names = FALSE)

### bind all top model averages together and export
getwd()
setwd("/home/rstudio/trophic csv")

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files

datalist <- list()
for (i in 1:6) {
  filename <- paste(getwd(), csv_files[i], sep = "/")
  mySheet <- read.csv(filename)
  trophic.group <- gsub("Biom_avg coefs.csv", "", paste(csv_files[i]))
  mySheet$trophic <- trophic.group
  datalist[[i]] <- mySheet
}  

big_data = do.call(rbind, datalist)
names(big_data)[1] <- "lower.95ci"
names(big_data)[2] <- "lower.95ci"
big_data

setwd("/home/rstudio")
write.csv(big_data, "trophic model avgs.csv", row.names = FALSE)

