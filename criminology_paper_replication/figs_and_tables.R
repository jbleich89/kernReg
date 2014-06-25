#########
## This script provides the code to reproduce the tables and figures in
##
## Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., 
## Using Regression Kernels to Forecast A Failure to Appear in Court. (2014)
## working paper
##
## This script is written by Adam Kapelner and Justin Bleich

#load("criminology_data.RData") ###this data is not released publically due to privacy concerns

###convert the data frame to a matrix
X$ThreeWayFail = ifelse(X$ThreeWayFail == "Low", 1, ifelse(X$ThreeWayFail == "Moderate", 2, 3)) #this decision is arbitrary and the levels can be changed if you wish
X$FailAny = ifelse(X$FailAny == "fail", 1, 0)
X$FailSerious = ifelse(X$FailSerious == "fail", 1, 0)
X = as.matrix(X)

##load library
library(kernReg)

### now we'll do the Fig X



explore_kpclr_obj = explore_kpclr_models(X, y, fp_cost = 2)
plot(explore_kpclr_obj, quantile_cwe_to_display = 0.99)
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)





