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

#kernel_list = list()
#kernel_list[[1]] = list(kernel_type = "anova", params = c(0.1, 2))
#kernel_list[[2]] = list(kernel_type = "anova", params = c(1, 2))
#kernel_list[[3]] = list(kernel_type = "anova", params = c(10, 2))
#kernel_list[[4]] = list(kernel_type = "anova", params = c(100, 2))
#kernel_list[[5]] = list(kernel_type = "anova", params = c(1000, 2))
#kernel_list[[6]] = list(kernel_type = "anova", params = c(10000, 2))
#kernel_list[[7]] = list(kernel_type = "anova", params = c(0.1, 3))
#kernel_list[[8]] = list(kernel_type = "anova", params = c(1, 3))
#kernel_list[[9]] = list(kernel_type = "anova", params = c(10, 3))
#kernel_list[[10]] = list(kernel_type = "anova", params = c(100, 3))
#kernel_list[[11]] = list(kernel_type = "anova", params = c(1000, 3))
#kernel_list[[12]] = list(kernel_type = "anova", params = c(10000, 3))



explore_kpclr_obj = explore_kpclr_models(X, y, fp_cost = 2)
plot(explore_kpclr_obj, quantile_cwe_to_display = 0.99)
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)







f1 = c(4.673194e-02, 1.682350e-01, 2.673067e-01, 2.527263e-01, 1.603840e-01,
		7.270742e-02, 2.437837e-02, 6.158746e-03, 1.180426e-03, 1.710763e-04,
1.847624e-05, 1.443262e-06, 7.713985e-08, 2.526665e-09, 3.828280e-11)
xhist <- hist(f1,breaks=20,plot=FALSE) 
yhist <- xhist$counts/sum(xhist$counts) 
yhist 
plot(xhist$breaks, yhist)
barplot(yhist, names.arg=xhist$breaks[1:27], xlab = "confused", ylab = "probability")



