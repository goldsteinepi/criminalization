#################
# HIV criminalization and structural stigma
# Citation: Tran NK, Hatzenbuehler ML, Goldstein ND. Potential Relationship between HIV Criminalization and Structural Stigma in the U.S. Manuscript in preparation.
# 8/24/18 -- Nguyen Tran & Neal Goldstein
#################

### DATA SOURCES ###

# Presence of Law: https://www.cdc.gov/hiv/policies/law/states/index.html
# Felony: http://www.hivlawandpolicy.org/sites/default/files/State-by-State%20Chart%20of%20HIV-Specific%20Statutes%20and%20Prosecutorial%20Tools.%20%28final%208%2023%2017%29.pdf
# Number of arrests: https://www.hivlawandpolicy.org/sites/default/files/Chart%20of%20U.S.%20Arrests%20and%20Prosecutions%20for%20HIV%20Exposure%20in%20the%20United%20States%20%28updated%20May%202018%29.pdf
# US census data: https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
# 2013 State and Legislative Partisan Composition (1 = republican; 2 = democrat; 3 = split): http://www.ncsl.org/research/about-state-legislatures/partisan-composition.aspx


### FUNCTIONS ###

library(cartogram) #Creating cartograms 
library(maptools)
library(maps)
library(RColorBrewer)

### READ DATA ###

setwd() # set working directory 

stigma = read.csv("criminalization_github.csv") # import dataset 

### DATA MANAGEMENT ###

# Rate of arrest by persons living with HIV per 100,000
stigma$Rate = (stigma$Arrest/stigma$X2016_HIV_pre) * 100000
  
# Flip the the signs of z scores to indicate stigma rather than social support 
stigma$stigma = stigma$Support_zscore * -1

# Recoded stigma into quartiles for sensitivity analysis 
stigma$stigmaqrt = ifelse(stigma$stigma<=-2.7163, 1, 
                          ifelse(stigma$stigma<=0.7876 & stigma$stigma>-2.7163, 2,
                                 ifelse(stigma$stigma<=3.1096 & stigma$stigma>0.7876, 3, 4)))

# Creating datasets for examining association of HIV criminlaization components
stigma_restrict = subset(stigma, Law == 1) #restricted to states with a law in place 

### Create cartogram ###

# https://www.census.gov/geo/maps-data/data/tiger-line.html 
us_state = readShapePoly("cb_2017_us_state_20m/cb_2017_us_state_20m")

# removing regions with NAs
contin_48 = subset(us_state, !(NAME %in% c("Alaska", "Hawaii", "Puerto Rico", "District of Columbia")))
plot(contin_48)

# merge stigma dataset with shapefile 
contin_48 = merge(x=contin_48, y=stigma, by="STUSPS", all.x=T)
head(contin_48@data)

# cholorpleth of Criminal law risk exposure and structural stigma 
spplot(contin_48, "Rate", cuts=4, col.regions=brewer.pal(5, "Reds"))
spplot(contin_48, "stigma", cuts=4, col.regions=brewer.pal(5, "Reds"))

# 1 variable cartogram for Criminal law exposure - itermax= indicates the amount of times the program distorts of shapes
us_carto = cartogram_cont(contin_48, "Rate", itermax=100)
plot(us_carto, main="distorted (sp)")

# 2 variable cartogram for Criminal law exposure and structural stigma 
spplot(us_carto, "stigma", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"))

### REGRESSION OF STRUCTURAL STIGMA BY 3 COMPONENTS OF HIV CRIMINALIZATION ###
summary(lm(Law ~ stigma + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma))
confint(lm(Law ~ stigma + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma))

summary(lm(Felony ~ stigma + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma_restrict))
confint(lm(Felony ~ stigma + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma_restrict))

summary(lm(Rate ~ stigma + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma_restrict))
confint(lm(Rate ~ stigma + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma_restrict))

### SENSITVITY ANALYSIS ###
summary(lm(Rate ~ stigmaqrt + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma_restrict))
confint(lm(Rate ~ stigmaqrt + Population + Med_age + Pct_blk + X2013_pol_aff, data = stigma_restrict))