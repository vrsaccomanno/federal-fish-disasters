# Brice X. Semmens, 04/16/20

# Statistics to be done:
# Eavluate the shift in causes of fishery disasters over time
# Approach: Ordered logistic regression, with responses (not human caused, partially 
# human caused, and fully human caused) as ordered categories and year as the explanatory 
# variable. Again, could include region as a random or fixed effect.

# Here’s a definition of each of field in the dataset:
#Disaster Number: The federal ID number given on the NOAA website
#Fishery(ies): The fishery or fisheries listed on the NOAA website
#State: State(s) or territories listed on the NOAA website
#Management Zone: NOAA Fisheries management region
#Determination Year: The year that each disaster was federally determined
#Impact year(s): The year(s) in which each disaster actually occurred
#Disaster Cause: The causes of disasters condensed into three broad categories
#Appropriation amount (2019 USD): Amount appropriated by Congress
#Net Revenue Change (2019 USD): Change in ex-vessel revenue during the disaster year relative to the previous 5-year average
#State-years: number of states and years impacted in each disaster

library(tidyverse)
### BRING IN DATA ####################################################################
# First let's read in the raw data file (observation records in flat format)
d.data <- read.csv("Disaster_Summary_Data_forBrice.csv", header = T)

############################# Let's now do Ordinal Logistic Regression ############################
#Build ordinal logistic regression model
d.data$Disaster.Cause = factor(d.data$Disaster.Cause, levels = c("Anthropogenic", "Combination of Both", "Environmental"), ordered = TRUE)

d.data$scaled.Year<-as.vector(scale(d.data$Determination.Year)) # to aid in convergence

library(MASS)
library(car)
library(effects)
model<- polr(Disaster.Cause ~ scaled.Year , data = d.data, Hess = TRUE)
summary(model)
Anova(model)

#Plotting the effects
plot.model<- polr(Disaster.Cause ~ Determination.Year , data = d.data, Hess = TRUE) #just to use non-scale year in plot
model.for.plot<-polr(Disaster.Cause ~ Determination.Year , data = d.data, Hess = TRUE)

# separate plots
plot(Effect(focal.predictors = "Determination.Year",plot.model),
     xlab="Determination Year", ylab="Disaster Cause (Probability)", main=NULL)
# stacked plot
plot(Effect(focal.predictors = "Determination.Year",plot.model),style="stacked",rug=TRUE,
     xlab="Determination Year", ylab="Disaster Cause (Probability)", main=NULL)


# Let's try a random effects version with 1|Management.Zone
library(ordinal)
model.r <- clmm2(Disaster.Cause ~ scaled.Year, random=Management.Zone, 
                 data=d.data,Hess = TRUE, nAGQ = 20)
summary(model.r)

# plotting work
library(ggplot2)
library(viridis)
library(hrbrthemes)

