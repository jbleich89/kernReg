library(kernReg)

#training data
X_train = as.matrix(Split2[, 5 : 49]) 
X_validate = as.matrix(Split1[, 5 : 49]) 
X_test = as.matrix(Split3[, 5 : 49]) 

#response
y_train = as.numeric(ifelse(Split2$FailToAppear=="YesFta",1,0)) 
y_validate = as.numeric(ifelse(Split1$FailToAppear=="YesFta",1,0)) 
y_test = as.numeric(ifelse(Split3$FailToAppear=="YesFta",1,0)) 

##Set up weights for plots
weights = weights_for_kpca_logistic_regression(y_train, 0.5)

##First see example that doesn't really seem to converge
### see https://stat.ethz.ch/pipermail/r-devel/2005-June/033508.html for better code
kpca1 = build_kpca_object(X_train, "anova", c(.0001, 2))
plot(kpca1, at = seq(0, 2000, by = 20))
kpca1 = build_kpca_object(X_train, "anova", c(.001, 2))
plot(kpca1, at = seq(0, 2000, by = 20))
kpca1 = build_kpca_object(X_train, "anova", c(.01, 2))
plot(kpca1, at = seq(0, 2000, by = 20))
kpca1 = build_kpca_object(X_train, "anova", c(.1, 2))
plot(kpca1, at = seq(0, 2000, by = 20))
kpca1 = build_kpca_object(X_train, "anova", c(10, 2))
plot(kpca1, at = seq(0, 2000, by = 20))
windows()
plot(kpca1, at = seq(0, 2000, by = 20))
kpca1 = build_kpca_object(X_train, "anova", c(100, 2))
plot(kpca1, at = seq(0, 2000, by = 20))

kpca1 = build_kpca_object(X_train, "anova", c(1e-10, 2))
plot(kpca1)
windows()
plot(kpca1, at = seq(0, 2000, by = 20))

#graphics.off()
#kpca_object = build_kpca_object(X_train, "rbf", c(0.00000001))
#plot(kpca_object)
#windows()
#kpca_object = build_kpca_object(X_train, "rbf", c(0.0001))
#plot(kpca_object)
#windows()
#kpca_object = build_kpca_object(X_train, "rbf", c(0.001))
#plot(kpca_object)
#windows()
#kpca_object = build_kpca_object(X_train, "rbf", c(0.01))
#plot(kpca_object)
#windows()
#kpca_object = build_kpca_object(X_train, "rbf", c(0.1))
#plot(kpca_object)
#windows()
#kpca_object = build_kpca_object(X_train, "rbf", c(1))
#plot(kpca_object)

rho_seq = seq(.05, .90, by = .05) ## seq of % variance to explain in kernel 

threshold = .5 ##threshold for positive class
err_mat = matrix(nrow = length(rho_seq), ncol = 2) ##
colnames(err_mat) = c("Class 0", "Class 1")#, "Cost-Weighted")
#kpca_logistic_regression = function(kpca_object, y, num_pcs = NULL, frac_var = NULL, weights = NULL){


for (i in 1 : length(rho_seq)){
  mod = kpca_logistic_regression(kpca1, y_train, frac_var = rho_seq[i], weights = weights) ##build model 
  p_hats = predict(mod, new_data = X_validate) ## predict on validation - Split1
  tab = table(factor(y_validate, levels = c(0, 1)), factor(as.numeric(p_hats > threshold), levels = c(0,1))) #build table
  fp = tab[1,2] / sum(tab[1,]) ##FP compute
  fn = tab[2,1] / sum(tab[2,]) ##FN compute
  err_mat[i,] = c(fp, fn) ##store errror
  print(i) ## counter
}

par(mgp = c(1.8, .5,0), mar = c(4.4, 2.7, 0.1, 0.1)) 
plot(rho_seq, err_mat[,1], type = "l", col = "blue", ylim = c(0,1), xlab = "Variance of Kernel Matrix", ylab = "Class Error")
points(rho_seq, err_mat[,2], type = "l", col = "red")
legend("topright", legend = c("No Fail", "Fail"), col = c("blue", "red"), lty = 1)






##Now show example that does seem to converge
library(kernReg)
kpca1 = build_kpca_object(X_train, "anova", c(10, 2))
plot_kpca_logistic_regression_perf(kpca1, y_train = y_train, y_validate = y_validate, X_validate = X_validate, weights = weights)


#try a bunch

kernel_list = list()
kernel_list[[1]] = list(kernel_type = "anova", params = c(0.01, 2))
kernel_list[[2]] = list(kernel_type = "anova", params = c(0.1, 2))
kernel_list[[3]] = list(kernel_type = "anova", params = c(1, 2))
kernel_list[[4]] = list(kernel_type = "anova", params = c(10, 2))
kernel_list[[5]] = list(kernel_type = "poly", params = c(3, 1, 0))
kernel_list[[6]] = list(kernel_type = "spline", params = c())
kernel_list[[7]] = list(kernel_type = "tanh", params = c(1, 0))
kernel_list[[8]] = list(kernel_type = "vanilla", params = c())
kernel_list[[9]] = list(kernel_type = "anova", params = c(1, 2))
results = kernel_finder_logistic_regression(kernel_list, X_train = X_train, y_train = y_train, X_validate = X_validate, y_validate = y_validate, weights = weights)

mod = kpca_logistic_regression(kpca1, y_train, frac_var = 0.75, weights = weights) ##build model 
p_hats = predict(mod, new_data = X_validate, type = "link")

plot_pdp(mod, X_train, predictor = "FollowUpYears", frac_to_build = 0.1)


##Now pick a model and evaluate on final holdout
##let's say .75 
np = get_num_pcs(kpca_object = kpca1, frac_var_to_explain = .75) ##based on plot
weights = weights_for_kpca_logistic_regression(y_train, 1/2)
threshold = .5

log_reg2 = kpca_logistic_regression(y = y_train, kpca_object = kpca1, num_pcs = np, weights = weights) #rebuild "best" model
split3_preds = kernReg_predict(log_reg2, new_data = X_test, training_data = X_train) #get some predictions
table(y_test, split3_preds > threshold) ##confusion table for holdout

par(mgp=c(1.8,.5,0), mar=c(4.4,2.7,0.1,0.1)) ##plot histogram
hist(split3_preds, col = "grey", breaks = 20, main = "", xlab = "Probability of Failure to Appear")
summary(split3_preds)

# save(split3_preds, file = "split3_fitted.Rdata")


##################################
##A Fun Simulated example 
library(kernReg)
set.seed(15)
n = 200
x = as.matrix(sort(runif(n, -10, 10)))
ce = sin(abs(x))/abs(x) - mean(sin(abs(x))/abs(x))
y = ce + rnorm(n, 0, .15)
windows()
par(mgp=c(1.8,.5,0), mar=c(4.4,2.7,0.1,0.1)) ##plot histogram
plot(x, y, pch = 16, xlab = "X", ylab = "Y") #training points
#points(x, ce, type = "l", col = "blue", lwd = 4) ##true cond. exp. function
xstar = as.matrix(seq(-10, 10, length.out = 500)) ##testing points

##M0
kpca0 = build_kpca_object(x, "rbf", 0.02)
mod0 = kpca_regression(kpca0, y, frac_var = 0.95) ##build model 
y_hats0 = predict(mod0, xstar) ##predict
points(xstar, y_hats0, col = "brown", type = "l", lwd = 3) ##plot

##M1
kpca1 = build_kpca_object(x, "rbf", 0.1)
mod1 = kpca_regression(kpca1, y, frac_var = 0.95) ##build model 
y_hats1 = predict(mod1, xstar) ##predict
points(xstar, y_hats1, col = "red", type = "l", lwd = 3) ##plot

##M2
kpca2 = build_kpca_object(x, "rbf", 5)
mod2 = kpca_regression(kpca2, y, frac_var = 0.95) ##build model 
y_hats2 = predict(mod2, xstar) ##predict
points(xstar, y_hats2, col = "forestgreen", type = "l", lwd = 3) ##plot

##M3
kpca3 = build_kpca_object(x, "rbf", 10)
mod3 = kpca_regression(kpca3, y, frac_var = 0.95) ##build model 
y_hats3 = predict(mod3, xstar) ##predict
points(xstar, y_hats3, col = "blue", type = "l", lwd = 3) ##plot

##M4
kpca4 = build_kpca_object(x, "rbf", 50)
mod4 = kpca_regression(kpca4, y, frac_var = 0.95) ##build model 
y_hats4 = predict(mod4, xstar) ##predict
points(xstar, y_hats4, col = "grey", type = "l", lwd = 3) ##plot

legend("topright", legend = paste("gamma =", c(0.1,5,50), sep = ""), col = c("red","forestgreen","grey"), lty = 1, lwd = 3)

windows()
plot(kpca0, col.regions = rainbow(200, end = 0.78))
windows()
plot(kpca1)
windows()
plot(kpca2)
windows()
plot(kpca3)
windows()
plot(kpca4)

##Alt version without color
library(kernReg)
set.seed(15)
n = 200
x = sort(runif(n, -10, 10))
ce = sin(abs(x))/abs(x) - mean(sin(abs(x))/abs(x))
y = ce + rnorm(n, 0, .15)
windows()
par(mgp=c(1.8,.5,0), mar=c(4.4,2.7,0.1,0.1)) ##plot histogram
plot(x, y, xlab = "X", ylab = "Y") #training points
#points(x, ce, type = "l", col = "blue", lwd = 4) ##true cond. exp. function
xstar = seq(-10, 10, length.out = 500) ##testing points

##M1
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(1/5,1)) ##use radial basis with gamma = 1/5
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #8 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 8) ##build model
p_hats = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,p_hats, col = "black", type = "l", lty = 1, lwd = 3) ##plot

##M2
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(2,1)) ##use radial basis with gamma = 2
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #24 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 24) ##build model
p_hats = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,p_hats, col = "black", type = "l", lty = 2, lwd = 3) ##plot

##M3
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(10,1)) ##use radial basis with gamma = 10
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #48 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 48) ##build model
p_hats = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,p_hats, col = "black", type = "l", lty = 3, lwd = 3) ##plot

legend("topright", legend = paste("gamma =", c(.2,2,10), sep = ""), lty = 1: 3, lwd = 3)



#new stuff 
library(kernReg)
X_train = matrix(c(1,2,3,4,5,6), ncol=3)
X_train = rbind(X_train, X_train + 2, X_train + 4)
ytrainr = seq(10,15)
ytrainc = c(1,0,1,0,1,0)
kpca1 = build_kpca_object(X_train, "anova", c(0.1, 2))
kpca1
kpcareg_mod = kpca_regression(kpca1, ytrainr, frac_var = 0.5)
kpcareg_mod
kpcalreg_mod = kpca_logistic_regression(kpca1, ytrainc, frac_var = 0.2)
kpcalreg_mod
