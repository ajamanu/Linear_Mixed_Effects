# Linear_Mixed_Effects.R
# Overview of Linear Mixed Effects models
# Based on A very basic tutorial for performing linear mixed effects analyses
# by Bodo Winter
# http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf 
# Created by Aja Manu by 26/06/2018

#### Prelim---------------------------------------------------------------------
# clear working environment
rm(list = ls())

# load library
library(lme4) # for linear mixed effects

#### Load Data------------------------------------------------------------------
# load data
politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

#### Analysis-------------------------------------------------------------------
# plot data: look at the relationship between politeness and pitch
boxplot(frequency ~ attitude*gender, col=c("white","lightgray"),politeness)


# LMEF model: the fixed effect “attitude” (polite vs.informal) to predict voice 
# pitch, controlling for by-subject and by-item variability
politeness.model <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), 
                         data=politeness)
summary(politeness.model)

# LMEF model: add gender as an additional fixed effect
politeness.model <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), 
                         data=politeness)

summary(politeness.model)

# test signifiance 
politeness.null = lmer(frequency ~ gender + (1|subject) + (1|scenario), 
                       data=politeness, REML=FALSE) # model without the factor that you’re interested

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario),
                        data=politeness, REML=FALSE) # the model with the factor that you’re interested

# Likelihood Ratio Test
anova(politeness.null,politeness.model)

# look at coefficients for randon intercept only model 
coef(politeness.model)

# This also incorporates a random slope as well
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject) + 
                              (1+attitude|scenario), data=politeness, REML=FALSE)

# look at coefficients for randon intercept only model 
coef(politeness.model)

# test signifiance
politeness.null = lmer(frequency ~ gender + (1+attitude|subject) + (1+attitude|scenario),
                       data=politeness, REML=FALSE)

# Likelihood Ratio Test
anova(politeness.null,politeness.model)
