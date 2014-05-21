##using our software
library(kernReg) ##load package

get_weights = function(y, fn_to_fp_ratio){
  n1 = sum(y == 1)
  n0 = sum(y == 0)
  weight0 = (n1 + n0)/(n0 * (fn_to_fp_ratio + 1))
  weight1 = (n1 + n0 - n0 * weight0)/n1
  weights = ifelse(y == 1, weight1, weight0)
  weights
}

setwd("C:/Users/jbleich/Dropbox/Research_Crim/kernels/") ##my directory
load("Split1.rdata"); load("Split2.rdata"); load("Split3.rdata")

Split1Design = as.matrix(Split1[, 5 : 49]) #  design matrix for kernlab -- validation split
Split2Design = as.matrix(Split2[, 5 : 49]) # design matrix for kernlab -- training split
Split3Design = as.matrix(Split3[, 5 : 49]) # design matrix for kernlab -- true holdout split

#response
Fail1 = as.numeric(ifelse(Split1$FailToAppear=="YesFta",1,0)) ## validation 
Fail2 = as.numeric(ifelse(Split2$FailToAppear=="YesFta",1,0)) ##training 
Fail3 = as.numeric(ifelse(Split3$FailToAppear=="YesFta",1,0)) ## holdout

##Set up weights for plots
weight_vec = get_weights(Fail2, 1/2)

##First see example that doesn't really seem to converge
k_params = c(.1, 2) ##set params for ANOVA radial basis function
Kobj = K_matrix(X = Split2Design, fun = k_anova_basis, params = k_params) ##build kernel object on split2
kpca_object = kernel_pca(K_object = Kobj) ## get kernel pca of object -- same call as kpca() essentially

var_seq = seq(.05, .90, by = .05) ## seq of % variance to explain in kernel 

threshold = .5 ##threshold for positive class
err_mat = matrix(nrow = length(var_seq), ncol = 2) ##
colnames(err_mat) = c("Class 0", "Class 1")#, "Cost-Weighted")

for(i in 1 : length(var_seq)){
  mod = kpca_logistic_regression(y = Fail2, kpca_object = kpca_object, num_pcs = get_num_pcs(kpca_object, var_seq[i]), weight_vec) ##build model 
  preds = kernReg_predict(object = mod, new_data = Split1Design, training_data = Split2Design) ## predict on validation - Split1
  tab = table(factor(Fail1, levels = c(0,1)), factor(as.numeric(preds > threshold), levels = c(0,1))) #build table
  fp = tab[1,2]/sum(tab[1,]) ##FP compute
  fn = tab[2,1]/sum(tab[2,]) ##FN compute
  err_mat[i,] = c(fp, fn) ##store errror
  print(i) ## counter
}

par(mgp=c(1.8, .5,0), mar=c(4.4, 2.7, 0.1, 0.1)) 
plot(var_seq, err_mat[,1], type = "l", col = "blue", ylim = c(0,1), xlab = "Variance of Kernel Matrix", ylab = "Class Error")
points(var_seq, err_mat[,2], type = "l", col = "red")
legend("topright", legend = c("No Fail", "Fail"), col = c("blue", "red"), lty = 1)


##Now show example that does seem to converge
k_params = c(2, 3) ##set params for ANOVA radial basis function
Kobj = K_matrix(X = Split2Design, fun = k_anova_basis, params = k_params) ##build kernel object on split2
kpca_object = kernel_pca(K_object = Kobj) ## get kernel pca of object -- same call as kpca() essentially

var_seq = seq(.05, .90, by = .05) ## seq of % variance to explain in kernel 

threshold = .5 ##threshold for positive class
err_mat = matrix(nrow = length(var_seq), ncol = 2) ##
colnames(err_mat) = c("Class 0", "Class 1")#, "Cost-Weighted")

for(i in 1 : length(var_seq)){
  mod = kpca_logistic_regression(y = Fail2, kpca_object = kpca_object, num_pcs = get_num_pcs(kpca_object, var_seq[i]), weights = weight_vec) ##build model 
  preds = kernReg_predict(object = mod, new_data = Split1Design, training_data = Split2Design) ## predict on validation - Split1
  tab = table(factor(Fail1, levels = c(0,1)), factor(as.numeric(preds > threshold), levels = c(0,1))) #build table
  fp = tab[1,2]/sum(tab[1,]) ##FP compute
  fn = tab[2,1]/sum(tab[2,]) ##FN compute
  err_mat[i,] = c(fp, fn) ##store errror
  print(i) ## counter
}


par(mgp=c(1.8, .5,0), mar=c(4.4, 2.7, 0.1, 0.1)) 
plot(var_seq, err_mat[,1], type = "l", col = "blue", ylim = c(0,1), xlab = "Variance of Kernel Matrix", ylab = "Class Error")
points(var_seq, err_mat[,2], type = "l", col = "red")
legend("topright", legend = c("No Fail", "Fail"), col = c("blue", "red"), lty = 1)


##Now pick a model and evaluate on final holdout
##let's say .75 
np = get_num_pcs(kpca_object = kpca_object, frac_var_to_explain = .75) ##based on plot
weight_vec = get_weights(Fail2, 1/2)
threshold = .5

log_reg2 = kpca_logistic_regression(y = Fail2, kpca_object = kpca_object, num_pcs = np, weights = weight_vec) #rebuild "best" model
split3_preds = kernReg_predict(log_reg2, new_data = Split3Design, training_data = Split2Design) #get some predictions
table(Fail3, split3_preds > threshold) ##confusion table for holdout

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
xstar = seq(-10, 10, length.out = 500) ##testing points

##M1
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(1/5,1)) ##use radial basis with gamma = 1/5
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #8 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 8) ##build model
preds = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,preds, col = "forestgreen", type = "l", lwd = 3) ##plot

##M2
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(2,1)) ##use radial basis with gamma = 2
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #24 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 24) ##build model
preds = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,preds, col = "red", type = "l", lwd = 3) ##plot

##M3
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(10,1)) ##use radial basis with gamma = 10
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #48 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 48) ##build model
preds = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,preds, col = "blue", type = "l", lwd = 3) ##plot

legend("topright", legend = paste("gamma =", c(.2,2,10), sep = ""), col = c("forestgreen","red","blue"), lty = 1, lwd = 3)


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
preds = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,preds, col = "black", type = "l", lty = 1, lwd = 3) ##plot

##M2
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(2,1)) ##use radial basis with gamma = 2
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #24 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 24) ##build model
preds = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,preds, col = "black", type = "l", lty = 2, lwd = 3) ##plot

##M3
kobj = K_matrix(X = as.matrix(x), k_anova_basis, c(10,1)) ##use radial basis with gamma = 10
kernregpca = kernel_pca(K_object = kobj) #run pca
get_num_pcs(kpca_object = kernregpca, .95) #48 pcs for 95%
kpr = kpca_regression(y = y, kpca_object = kernregpca, num_pcs = 48) ##build model
preds = kernReg_predict(object = kpr, new_data = as.matrix(xstar), training_data = as.matrix(x)) ##predict
points(xstar,preds, col = "black", type = "l", lty = 3, lwd = 3) ##plot

legend("topright", legend = paste("gamma =", c(.2,2,10), sep = ""), lty = 1: 3, lwd = 3)

