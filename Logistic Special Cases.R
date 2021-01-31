##### START BLOCK 1 #######

#Purposes: 
#1) Detect missing values, replace with "MISSING" category
#2) Detect separation issues, recategorize variables as required.

#Load required libraries
library(haven)
library(tidyverse)
library(MASS)
library(car)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)

#Read in the dataset
dat = read_sas("insurance_v_bin.sas7bdat")

#Separate the target and predictor variables
targ = dat %>% select(INS)
preds = dat %>% select(-INS)

### Dealing with missing values

#Are any of the target value entries missing?
any(is.na(targ$INS)) #No

#Which of the predictor variables are missing values?
apply(preds, 2, function(i){sum(is.na(i))}) %>% 
  sort(decreasing = TRUE ) %>% 
  .[.>0]
#4 variables: HMOWN, INV, CC, CCPURC

#Exploring each of these variables, to determine how missing values should be coded
unique(preds$HMOWN) #Binary
unique(preds$INV) #Binary
unique(preds$CC) #Binary
unique(preds$CCPURC) #Ordinal
#Variables with missing values are mostly binary or ordinal. To remove missing values, we will reclassify the variables as character, replace NA with "MISSING", convert the variables to factors, and then relevel the factors with 0 as the reference category and "MISSING" as the last category.

#Since all of the variables are going to have to be factorial eventually anyway, we will do this for all predictor variables at once.

#Code to factorize (categorize) all variables, and replace missing values with a "MISSING" category where necessary.
#Create an output data frame
preds_fac = preds
#Loop through all predictor columns:
for(j in 1:ncol(preds)){
  #Convert the variable to a character
  char_var = preds[,j] %>% unlist() %>% as.character()
  #Extract the levels of the current column
  lvls = unique(char_var) %>% unlist()
  #Are there any missing values? If so, replace them with missing
  if(any(is.na(lvls))){
    char_var[which(is.na(char_var))] = "MISSING"
    lvls = unique(char_var) %>% unlist()
  }
  #Sort the levels
  lvls = lvls %>% sort()
  #Create a factorial version of the variable and apply the new level scheme
  fac_var = factor(char_var, levels = lvls)
  #Add the factorial column to the output data frame
  preds_fac[,j] = fac_var
}
# The BRANCH case: Branch has a whole bunch of factor levels that don't sort easily. I want the branches in order, so I'm going to do this manually:
branch.levels = unique(preds$BRANCH) %>% str_remove(pattern = "B") %>% as.numeric %>% sort() %>% paste0("B",.)
preds_fac$BRANCH = factor(preds_fac$BRANCH, levels = branch.levels)

### Separation concerns
#For each predictor, make a table of the target vs the levels of the predictor variables.If any entry in this table is 0, flag the variable as having potential separation issues
#Make a list for the flag variable
sep_issues = rep(0, ncol(preds_fac))
#Loop through the columns:
for(j in 1:ncol(preds_fac)){
  #Make the table:
  tab = table(targ$INS, preds_fac[,j] %>% unlist())
  #If any entry in the table is 0, flag the variable:
  if(any(tab == 0)){
    sep_issues[j] = 1
  }
}
#Which columns have potential separation issues?
colnames(preds_fac)[sep_issues == 1]
#Only CASHBK and MMCRED, it would seem

#CASHBK:
table( preds_fac$CASHBK, targ$INS, dnn = c("CASHBK","INS"))
#CASHBK (Cash back requests) has 2 non-zero levels, 1 and 2. Only 2 customers had 2 cashback requests, and neither bought the annuity product. We will combine 1 and 2 into a single category, ">0". Combining the 2 zero observations in CASHBK == 2 with the 102 zero in CASHBK == 1 should not result in a serious loss in information.

#Extract the cashback variable, convert to character:
new_CASHBK = preds$CASHBK %>% as.character()
#Recode the variable where CASHBK = 1 or 2
new_CASHBK[new_CASHBK %in% c("1","2")] = ">0"
preds_fac$CASHBK = factor(new_CASHBK, levels = c("0",">0"))

#MMCRED:
table( preds_fac$MMCRED, targ$INS, dnn = c("MMCRED","INS"))
#MMCRED (Money market account credits) has 4 nonzero levels: 1,2,3, and 5. Level 5 has 1 zero and 0 ones. We're going to combine it with level 3, which has 4 zeros and 5 ones. This will more or less preserve the target variable structure in level 3 (roughly 50/50). Level 5 will be lost, but with only a single observation it's difficult to say if that really represents a major loss of information
#Extract the variable, convert to character, recode the variable where MMCRED = 3 or 5 to ">2", then refactor
new_MMCRED = preds$MMCRED %>% as.character()
new_MMCRED[new_MMCRED %in% c("3","5")] = ">2"
preds_fac$MMCRED = factor(new_MMCRED, levels = c("0","1","2",">2"))

#Create validation set
fac_df_v = bind_cols(preds_fac, targ)



#Start here
#Save data set
#save(fac_df_v, file = "insurance_t_refac_v.RData")

load(fac_df_v, file = "insurance_t_refac_v.RData")
terms_string = "DDA + NSF + IRA + INV + ILS + MTG + CC + DDABAL_Bin + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin + MMBAL_Bin"

#Step 3) Create the 1st order terms string
terms_string2 = paste(terms_string, "+ DDA*IRA")

#Step 4) Paste the terms string and the interaction string together into a formula string:
form_string2 = paste0("INS ~ ", terms_string2)

#Step 5) Convert the formula string into a formula object:
forward_formula2 = as.formula(form_string2)

#Create validation model
full_val = glm(forward_formula2,
               data = fac_df_v, family = binomial(link = "logit"))

fac_df_v$p_hat = predict(full_val, type="response")

#Confusion matrix
confus_m = confusionMatrix(fac_df_v$INS, fac_df_v$p_hat, threshold = 0.2975857 )

#Accuracy
accuracy = (confus_m[[1]][1]+confus_m[[2]][2])/nrow(fac_df_v)

#predict values of validation model
pred = prediction(fitted(full_val), factor(fac_df_v$INS))

#lift
perf = performance(pred, measure = 'lift', x.measure = 'rpp')

plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Validation Set")
abline(h=1, lty = 3)
