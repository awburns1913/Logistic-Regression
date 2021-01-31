library(MASS)
library(visreg)
library(brglm)
library(car)
library(mgcv)
library(haven)
library(dplyr)


ins = read_sas("insurance_t.sas7bdat")


#Test linearity assumption #
fit.gam <- gam(INS ~ s(MMCRED), data=ins, family = binomial(link='logit'),
               method = 'REML')
summary(fit.gam)


ins2 = ins %>% filter(!(is.na(PHONE)))
# Logistic Regression Model #
logit.model <- glm(INS ~ factor(ATM), 
                   data = ins, family = binomial(link = "logit"))
#summary(logit.model)

#Reduced model compared to above #
logit.model.r <- glm(INS ~ 1, 
                     data = ins, family = binomial(link = "logit"))

# Tests whether race is a significant variable in the regression #
# LRT is likelihood ratio test #
anova(logit.model, logit.model.r, test = 'LRT')
pvalue.DDA <- anova(logit.model, logit.model.r, test = 'LRT')$"Pr(>Chi)"[2]
pvalue.DDA





