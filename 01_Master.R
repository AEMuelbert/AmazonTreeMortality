#*******
# Codes for analyses from Esquivel-Muelbert et al. 2020 How and Why Amazon trees die
# Developed by Adriane Esquivel Muelbert
#*******

#packages 
library (maps); library (mapdata); library (mapplots); library (plotrix)
library (plyr); library (multicomp)
library (survival); library(lme4); library (MASS); library (rms)

source ('02_functions_plot_level.R')
source ('03_functions_survival_analyses.R')

 # data for plot-level analyses 
plot_rates <- read.csv ('d01_plot_rates.csv') # mortality rates per census
census_rates <- read.csv ('d02_census_rates.csv') # mortality rates per census
mod <- read.csv ('d03_census_mod.csv')
S <- read.csv ('d04_surv_matrix.csv')


# 1. Spatial distribution of tree death -------

# a. Plotting map of distribution of tree mortality across the Amazon ------
MapMortalityRates (plot_rates)

# b. Calculating the proportion of mode of death -------
# here using census interval as a response variable to account for its effect on the proportion of standing and broken/uprooted trees

# WHOLE BASIN
# model for non-standing (i.e. broken/uprooted) death
mNS <- lm (p.MoD.notstanding ~   census.interval.m0 , data = mod, na.action = na.omit)
summary (mNS) 
confint(mNS) # calculate confidence intervals 
# model for standing death
mS <- lm (p.MoD.lik.standing ~   census.interval.m0 , data = mod, na.action = na.omit)
summary (mS)
confint(mS)

# PER REGION
# calculate proportion of non-standing trees per region
mNSr <- lm (p.MoD.notstanding ~ 0 + census.interval.m0 + region , data = mod, na.action = na.omit)
summary (mNSr)
confint(mNSr)

# calculate proportion of standing trees per region
mSr <- lm (p.MoD.lik.standing ~  0 + census.interval.m0 + region , data = mod, na.action = na.omit)
summary (mSr)
confint(mSr)

# Applying posthoc test to test for the difference across regions
# Refit model with intercept to allow for the post-hoc test
mod$region<- as.factor (mod$region)

mNSr2 <- lm (p.MoD.notstanding ~ census.interval.m0 + region , data = mod, na.action = na.omit)
mSr2 <- lm (p.MoD.lik.standing ~ census.interval.m0 + region , data = mod, na.action = na.omit)

cld(glht(mNSr2,linfct=mcp(region="Tukey")))
cld(glht(mSr2,linfct=mcp(region="Tukey")))

# c. Plotting figure 2 - proportion and mode of death for different regions ----
CompareRegions (mod,plot_rates, census_rates) # This figures also use the proportions 



#******************************
# 2. Survival analyses -------
#******************************

S_ind <- Surv (time =S$interval, event = S$dead)

mFULL.Plot <- coxph (S_ind ~ poly (D40, 2)+
                       DrelGR0 + 
                       MaxD + 
                       MeangrD +
                       WD + 
                       WDA.gen+
                       frailty(Plot.Code, method = 'aic', sparse=10),data=S)

stepAIC (mFULL.Plot, direction = 'both')

# 2.2 comparing models (Table 1) -----
CompareSurvivalModels (S, event = 'dead')

# 2.3 split in different regions (Table 2) -----
# spliting data
SWA <- S [which (S$region == 'AMA_W'),]
SBS <- S [which (S$region == 'AMA_BrSh'),]
SEC <- S [which (S$region == 'AMA_EC'),]
SGS <- S [which (S$region == 'AMA_GuSh'),]

# creating Surv obj
SurvWA <- Surv (time =SWA$interval, event = SWA$dead)
SurvBS <- Surv (time = SBS$interval,event = SBS$dead)
SurvEC <- Surv (time = SEC$interval, event = SEC$dead)
SurvGS <- Surv (time = SGS$interval, event = SGS$dead)

# models for the different regions
mWA <- coxph (SurvWA ~ poly (D40, 2)+
                DrelGR0 + 
                MaxD +
                MeangrD +
                WD + 
                WDA.gen+
                frailty(Plot.Code),data=SWA)

mBS <- coxph (SurvBS ~ poly (D40, 2)+
                DrelGR0 + 
                MaxD +
                MeangrD +
                WD + 
                WDA.gen+
                frailty(Plot.Code),data=SBS)

mEC <- coxph (SurvEC ~ poly (D40, 2)+
                DrelGR0 + 
                MaxD +
                MeangrD +
                WD + 
                WDA.gen+
                frailty(Plot.Code),data=SEC)

mGS <- coxph (SurvGS ~ poly (D40, 2)+
                DrelGR0 + 
                MaxD +
                MeangrD +
                WD + 
                WDA.gen+
                frailty(Plot.Code),data=SGS)

# comparing regions
ciRegions <- CompareCI (mBS, mWA, mEC, mGS)
PlotCompareCI (ciRegions)

#plot survival models 
models <- list (mGS,  mEC, mWA,mBS, mFULL.Plot)
data <- list (SBS, SEC, SGS, SWA, S)

PlotSurvReg (models, data)



