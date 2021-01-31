library(haven)
library(tidyverse)

#creates full model for backwards elimination. "." notation means every variable not including INS. Since all variables are already factors, no need to use factor()
full.model = glm(INS ~ ., data=fac_df, family=binomial(link="logit") )
summary(full.model)

#This set of code is used for running the backwards selection method with a p-val of .002. The qchisq function is used to find the new k value for the step function which finds the equivalent AIC DOF for a p-val of .002
kbac = qchisq(.002, 1, lower.tail = FALSE)
back.model <- step(full.model, direction = "backward", k = kbac)
summary(back.model)

#Showing why CCMissing has NA for coefficients
alias(back.model)
table(fac_df$POS_Bin, fac_df$POSAMT_Bin)

#retrieves names of variables in final model
pvalname = names(back.model$coefficients)

#takes (intercept) out of the final var name list
pvalname = pvalname[-1]

#returns p-vals in final model
pval = coef(summary(back.model))[,4]

#combines list to new dataframe for csv export
df2 = data.frame(pvalname, pval)

#exports csv for our use
#write_csv(df2, "/Users/awburns2/Library/Mobile Documents/com~apple~CloudDocs/AnalyticsPackages/MSAR/Logistic Regression Data Sets/HW2_pval.csv")

