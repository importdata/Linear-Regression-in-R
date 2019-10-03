# Mini Project 2 - Linear Regression (Prostate Cancer)
# Name: Jaemin Lee
# NetID: JXL142430

############################### (A) exploratory data analysis ###############################

# open prostate cancer data file
prostate = read.csv("C:/Users/jaemi/Desktop/STAT 4360/Projects/Project 2/prostate_cancer.csv")

# look at the details in the data 
head(prostate); str(prostate)
  # notice vesinv is a categorical variable (0 and 1)
  # notice gleason only has 6, 7, and 8

# apply factor to vesinv so we can treat it correctly
vesinv.factor = factor(prostate$vesinv)

# remove the first column (subject variable)
prostate = prostate[,-1]

# summary of prostate data
summary(prostate)

# check the plot to see relationships among the variables
plot(prostate) # can't see any strong relationship between PSA and other variables 
               # maybe a good indicator that need some kind of transformation on PSA

###### (B) Is psa appropriate as a response variable or a transformation is necessary? ######

# look at PSA levels
prostate$psa # ranges from 0.651 to 265.072
  # Safe: 0 to 2.5 ng/mL
  # 2.6 to 4 ng/mL is safe in most men but talk with your doctor about other risk factors
  # Suspicious: 4 to 10 ng/mL. There's a 25% chance you have prostate cancer.
  # Dangerous: 10 ng/mL and above. There's a 50% chance you have prostate cancer.

# PSA vs cancervol - PSA as a response
prostate$cancervol 
plot(prostate$cancervol, prostate$psa, main ='PSA vs Cancer Volume', pch = 20) # hard to see the relationship

# PSA vs cancervol using log transformation on PSA
psa.log = log(prostate$psa)
plot(prostate$cancervol, psa.log, main = 'PSA.log vs Cancer Volume', pch = 20) # the graph looks better but maybe apply sqrt on cancervol?

cancervol.sqrt = sqrt(prostate$cancervol)
plot(cancervol.sqrt, psa.log, main = 'PSA.log vs Cancer Volume.sqrt', pch=20) # notice the plot has a linearity after the transformation

# PSA vs age 
prostate$age
plot(prostate$age, prostate$psa) # hard to see the relationship

# PSA vs age using log transformation on PSA
plot(prostate$age, psa.log) # notice the plot has some linearity after the transformation

# PSA vs weight 
plot(prostate$weight, prostate$psa) # there's no obvious trend

# PSA vs weight using log transformation on PSA
prostate$weight
plot(prostate$weight, psa.log) # there's a correlation between PSA and weight
                               # but maybe it can look better after applying sqrt on weight?
weight.sqrt = sqrt(prostate$weight)
plot(weight.sqrt, psa.log) # indeed, the graph looks much better

  # The log transformation seems to be necessary on PSA, 
  # because graphs look to represent better relationships after the transformation. 

##### (C) For each predictor, fit a simple linear regression model to predict the response. ###

# linear regression between PSA and cancervol
plot(psa.log~cancervol.sqrt, prostate)
fit1 = lm(psa.log~cancervol.sqrt, data = prostate)
fit1  # cancervol (slope) = 0.6366 -> some correlation
summary(fit1) # p-value for cancervol is less than 0.05 which is statistically significant
              # t-value for cancervol is greater than 2 which is significant
              # R^2 = 0.5145-> somewhat reliable
abline(fit1)  # linear regression line 

# linear regression between PSA and weight
plot(psa.log~weight.sqrt, prostate)
fit2 = lm(psa.log~weight.sqrt, data = prostate)
fit2  # slope = 0.1395 -> there is a correlation
summary(fit2) # p-value for weight.sqrt is less than 0.05 which is statistically significant
              # t-value for weight.sqrt is greater than 2 which is significant
              # R^2 = 0.061 -> not reliable
abline(fit2)

# linear regression between PSA and age
plot(psa.log~age, prostate)
fit3 = lm(psa.log~age, data = prostate)
fit3 # slope = 0.02633 -> weak correlation
summary(fit3) # p-value for age is greater than 0.05 which is NOT significant
              # t-value for age is less than 2 which is NOT significant
              # R^2 = 0.02887 -> not reliable
abline(fit3)

# linear regression between PSA and benpros
plot(psa.log~benpros, prostate)
fit4 = lm(psa.log~benpros, data = prostate)
fit4 # slope = 0.05991 -> weak correlation
summary(fit4) # p-value for benpros is greater than 0.05, which is NOT significant
              # t-value for benpros is less than 2, which is NOT significant
              # R^2 = 0.02478 -> not reliable
abline(fit4)

# linear regression between PSA and vesinv 
par(mfrow=c(1,1))
plot(psa.log~vesinv.factor, prostate)
fit5 = lm(psa.log~vesinv.factor, data = prostate)
fit5 # slope = 1.578 -> strong correlation
summary(fit5) # p-value for vesinv is less than 0.05, which is statistically significant
              # t-value for vesinv is greater than 2, which is significant
              # R^2 = 0.32 -> somewhat reliable
              # you can see there is a significant relationship from the box plot

# linear regression between PSA and capspen
plot(psa.log~capspen, prostate) # maybe plot looks better after applying sqrt on capspen?
capspen.sqrt = sqrt(prostate$capspen)
plot(psa.log~ capspen.sqrt, prostate) # indeed, the graph looks better
fit6 = lm(psa.log~capspen.sqrt, data = prostate)
fit6 # slope = 0.550 -> some correlation
summary(fit6) # p-value for capspen is less than 0.05, which is statistically significant
              # t-value for capspen is greater than 2, which is significant
              # R^2 = 0.30 -> somewhat reliable
abline(fit6)

# linear regression between PSA and gleason 
plot(psa.log~gleason, prostate) 
fit7 = lm(psa.log~gleason, data = prostate)
fit7 # slope = 0.8408 -> high correlation
summary(fit7) # p-value for capspen is less than 0.05, which is statistically significant
              # t-value for capspen is greater than 2, which is significant
              # R^2 = 0.29 -> somewhat reliable
abline(fit7)

  #### In conclusion, cancervol, weight, vesinv, capspen, and gleason are the variables that are
  #### significant predictors in psa level

##### (D) Fit a multiple regression model to predict the response using all of the predictors. #####
fit8 = lm(psa.log ~ .-psa, data = prostate) # remove psa as a variable as it's a response

summary(fit8) # cancervol, benpros, vesinv, gleason have p-values less than 0.05
              # when p-values are less than 0.05, then we can reject the null hypothesis
              # thus we can reject the null hypothesis for those 4 predictors above

              # the only difference between simple linear model and multiple regression model
              # is that weight is not significant anymore for multiple regression
              # also notice that R^2 = 0.557. use this to compare with my final model

par(mfrow=c(2,2))
plot(fit8) # notice Residuals vs Fitted has a slight curve due to the non-linearity
           # but it still looks good
           # Q-Q plot looks really good

##### (E) Build a "reasonably good" multiple regression model for these data. #####

# we want to look at R^2 to check if the model is reasonably good or not
# if R^2 is close to 1, then the model is reasonable
# if the model is close to 0, then the model is not reasonable

# apply interactions between predictors to fit a reasonably good model
# potential candidate for predictors: cancervol, vesinv, capspen, and gleason

# apply interactions between cancervol and gleason
fit9 = lm(psa.log ~ cancervol.sqrt*gleason, data=prostate)
fit9 # negative slope -> weak correlation
summary(fit9) # adjusted R^2 = 0.5399

# apply interactions between cancervol and benpros
fit10 = lm(psa.log ~ cancervol.sqrt*benpros, data=prostate)
fit10 # slope = -0.022 -> weak correlation
summary(fit10) # adjusted R^2 = 0.5466 

# apply interactions between cancervol and weight
fit11 = lm(psa.log ~ cancervol.sqrt*weight.sqrt, data=prostate)
fit11 # slope = -0.008273 -> weak correlation
summary(fit11) # adjusted R^2 = 0.53

# apply interactions between cancervol and vesinv
fit12 = lm(psa.log ~ cancervol.sqrt*vesinv.factor, data=prostate)
fit12 # slope = -0.205 -> weak correlation
summary(fit12) # adjusted R^2 = 0.537

# apply interactions between cancervol and capspen
fit13 = lm(psa.log ~ cancervol.sqrt*capspen.sqrt, data=prostate)
fit13 # slope = 0.297 -> some correlation
summary(fit13) # adjusted R^2 = 0.51

  ### all the above have R^2 around 0.5
  ### maybe apply quadratic term and add 5 of the candidate predictors to get better R^2

# apply interactions among the candidate predictors 
# apply interactions between cancervol and benpros, because it had the highest adjusted R^2
fit14 = lm(psa.log ~ cancervol.sqrt  + cancervol.sqrt*benpros + vesinv.factor + capspen.sqrt + gleason, data=prostate)
summary(fit14) # adjusted R^2 = 0.604 -> a lot better than previous models

  # fit15 is my final model
  # compared to the multiple regression model with all predictors, R^2 went up from 0.557 to 0.604

##################### (F) Write the final model in equation form #########################

# psa.log = -1.23129 + (0.52993 * cancervol.sqrt) + (0.16209 * benpros) + (0.69893 * vesinv.factor1)
#          + (-0.09462 * capspen.sqrt) + (0.32221 * gleason) + (-0.03576 * cancervol.sqrt:benpros)                             
#         with vesinv = 1, if seminal vesicle is present
#              vesinv = 0, if seminal vesicle is absent

##### (G) Use the final model to predict the PSA level for a patient whose quantitative predictors are at
#####     the sample means of the variables and qualitative predictors (if any) are at the most frequent category.

# find the mean of necessary variables
mean(cancervol.sqrt) # 2.307854
mean(cancervol.sqrt*prostate$benpros) # 5.584535
mean(prostate$benpros) # 2.534725
mean(capspen.sqrt) # 0.9627874
mean(prostate$gleason) # 6.87 -> round up to 7 as there are only 6, 7, and 8 
table(prostate$gleason) # run table to make sure 7 is occuring the most, and it is true as table 7 occurs 43 times
                        # whereas 6 occurs 33 and 8 occurs 21 times
# apply the table function to check frequency of vesinv
table(vesinv.factor) # 0 is more frequent as it occurs 76 times, whereas 1 occurs 21 times

# THE "hard" way
coef(fit14)
-1.23129 + (0.52993 * 2.307854) + (0.16209 * 2.534725) + (0.69893 * 0) + (-0.09462 * 0.9627874) + (0.32221 * 7) + (-0.03576 * 5.584535) 
  # psa level = 2.367233

# final fit
fit14 = lm(psa.log ~ cancervol.sqrt  + cancervol.sqrt*benpros + vesinv + capspen.sqrt + gleason, data=prostate)
predict(fit14, newdata=data.frame(cancervol.sqrt = 2.307854, vesinv = 0, capspen.sqrt = 0.9627874, gleason = 7, benpros = 2.534725), se.fit=T)
  # psa level = 2.357729  
# psa levels are off by 0.01. I am assuming this is happening because of the interactions in fit14. 
# either mean(cancervol.sqrt*prostate$benpros) is not accurate or predict() function doesn't include interactions as parameters.
