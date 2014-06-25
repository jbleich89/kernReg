#########
## This script provides the code to reproduce the tables and figures in
##
## Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., 
## Using Regression Kernels to Forecast A Failure to Appear in Court. (2014)
## working paper
##
## This script is written by Adam Kapelner and Justin Bleich

##load library
library(kernReg)

## Section 2
##

### Figure 5
set.seed(15)
n = 200
x = as.matrix(sort(runif(n, -10, 10)))
ce = sin(abs(x)) / abs(x) - mean(sin(abs(x)) / abs(x))
y = ce + rnorm(n, 0, .15)
par(mgp=c(1.8,.5,0), mar=c(2.7, 2.7, 0.1, 0.1)) ##plot histogram
plot(x, y, pch = 16, xlab = "X", ylab = "Y") #training points
#points(x, ce, type = "l", col = "blue", lwd = 4) ##uncomment if you want to see true cond. exp. function
xstar = as.matrix(seq(-10, 10, length.out = 500)) ##testing points

gamma_seq = c(1, 10, 500)
color_seq = c("firebrick3", "forestgreen", "grey")

kpca1 = build_kpca_object(x, "anova", c(gamma_seq[1], 1))
mod1 = kpcr(kpca1, y, frac_var = 0.95) ##build model 
y_hats1 = predict(mod1, xstar) ##predict
points(xstar, y_hats1, col = color_seq[1], type = "l", lwd = 3) ##plot

kpca2 = build_kpca_object(x, "anova", c(gamma_seq[2], 1))
mod2 = kpcr(kpca2, y, frac_var = 0.95) ##build model 
y_hats2 = predict(mod2, xstar) ##predict
points(xstar, y_hats2, col = color_seq[2], type = "l", lwd = 3) ##plot

kpca3 = build_kpca_object(x, "anova", c(gamma_seq[3], 1))
mod3 = kpcr(kpca3, y, frac_var = 0.95) ##build model 
y_hats3 = predict(mod3, xstar) ##predict
points(xstar, y_hats3, col = color_seq[3], type = "l", lwd = 3) ##plot

legend("topright", legend = paste("gamma =", gamma_seq), col = color_seq, lty = 1, lwd = 3)



## Section 3
##
library(kernReg)

#load("criminology_data.RData") ###this data is not released publically due to privacy concerns
###convert the data frame to a matrix
X$ThreeWayFail = ifelse(X$ThreeWayFail == "Low", 1, ifelse(X$ThreeWayFail == "Moderate", 2, 3)) #this decision is arbitrary and the levels can be changed if you wish
X$FailAny = ifelse(X$FailAny == "fail", 1, 0)
X$FailSerious = ifelse(X$FailSerious == "fail", 1, 0)
X = as.matrix(X)

### Figure 7
explore_kpclr_obj = explore_kpclr_models(X, y, fp_cost = 2)
#use the plot function to visualize all model choices
par(mar = c(4,4,3,2))
plot(explore_kpclr_obj, tile_cols = 2)
#pick a model holistically based on many considerations outlined in the paper
explore_kpclr_obj = set_desired_model(explore_kpclr_obj, winning_kernel_num = 2, winning_rho_num = 9)
#plot again so the desired model is marked with a blue line
plot(explore_kpclr_obj, tile_cols = 2)
#not in paper but almost made it: use the auto-selection method (not recommended)
explore_kpclr_obj = auto_select_best_kpclr_model(explore_kpclr_obj)
plot(explore_kpclr_obj, tile_cols = 2)

### Figure 8
#evaluate on the test data
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)
#plot a histogram of the estimated probabilities
par(mar = c(4,4,0,1))
hist(explore_kpclr_obj$p_test_hat, br = 50, xlab = "Predicted Probability of an FTA", main = "")

### Table 2
#the evaluation on the test data is already complete, pull out the confusion matrix and operate on that object
conf = explore_kpclr_obj$test_confusion
conf #rows 1,2 and cols 1,2
conf[1, 2] / sum(conf[1, ]) #col 3, row 1
conf[2, 1] / sum(conf[2, ]) #col 3, row 2
conf[2, 1] / conf[1, 2] #out-of-sample cost ratio (for the text)
conf[2, 1] / sum(conf[, 1]) #other fraction needed for text 

