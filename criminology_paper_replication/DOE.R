##DOE Example
setwd("C:/Users/jbleich/Dropbox/Research_Crim/DOE")
load("fullDOE.Rdata")
library(kernReg)
library(randomForest)
library(bartMachine)

full_data = read.csv("rf_model_data_w_updated_wasEverHomeless.csv", header = T)
full_data$Response = ifelse(full_data$Response == "Graduate", 1, 0)
set.seed(11)
samp_ix = sample(1 : nrow(full_data), 1500, F)
X = full_data[samp_ix, vars_list[-19]] ##vars_list is loaded from above 
y = full_data$Response[samp_ix]

data = red_data[samp_ix, ]
Xd = as.matrix(dummify_data(X))
dim(Xd)
length(y)
summary(Xd)
colnames(Xd)

# kernel_list = list()
# # kernel_list[[1]] = list(kernel_type = "anova", params = c(.1, 3))
# kernel_list[[1]] = list(kernel_type = "anova", params = c(1, 3))
# kernel_list[[2]] = list(kernel_type = "anova", params = c(10, 3))
# kernel_list[[3]] = list(kernel_type = "anova", params = c(100, 3))

kernel_list = list()
kernel_list[[1]] = list(kernel_type = "anova", params = c(0.1, 2))
kernel_list[[2]] = list(kernel_type = "anova", params = c(1, 2))
kernel_list[[3]] = list(kernel_type = "anova", params = c(10, 2))
kernel_list[[4]] = list(kernel_type = "anova", params = c(100, 2))
kernel_list[[5]] = list(kernel_type = "anova", params = c(1000, 2))
kernel_list[[6]] = list(kernel_type = "anova", params = c(10000, 2))
kernel_list[[7]] = list(kernel_type = "anova", params = c(0.1, 3))
kernel_list[[8]] = list(kernel_type = "anova", params = c(1, 3))
kernel_list[[9]] = list(kernel_type = "anova", params = c(10, 3))
kernel_list[[10]] = list(kernel_type = "anova", params = c(100, 3))
kernel_list[[11]] = list(kernel_type = "anova", params = c(1000, 3))
kernel_list[[12]] = list(kernel_type = "anova", params = c(10000, 3))
kernel_list[[13]] = list(kernel_type = "anova", params = c(0.1, 4))
kernel_list[[14]] = list(kernel_type = "anova", params = c(1, 4))
kernel_list[[15]] = list(kernel_type = "anova", params = c(10, 4))
kernel_list[[16]] = list(kernel_type = "anova", params = c(100, 4))
kernel_list[[17]] = list(kernel_type = "anova", params = c(1000, 4))
kernel_list[[18]] = list(kernel_type = "anova", params = c(10000, 4))
kernel_list[[19]] = list(kernel_type = "anova", params = c(0.01, 3))
kernel_list[[20]] = list(kernel_type = "anova", params = c(0.001, 3))
kernel_list[[21]] = list(kernel_type = "anova", params = c(0.0001, 3))
kernel_list[[22]] = list(kernel_type = "anova", params = c(0.00001, 3))
kernel_list[[23]] = list(kernel_type = "anova", params = c(0.000001, 3))
kernel_list[[24]] = list(kernel_type = "anova", params = c(0.0000001, 3))

explore_kpclr_obj = explore_kpclr_models(Xd, y, kernel_list = kernel_list, fn_cost = 5, fp_cost = 1)
#fn_max_cost = 6, fn_min_cost = 4, fp_max_cost = 1, fp_min_cost = 1
windows()
plot(explore_kpclr_obj, quantile_cwe_to_display = 0.99, plot_tile_cols = 4)
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)
explore_kpclr_obj$test_confusion
explore_kpclr_obj$fn_over_fp_validation_results


explore_kpclr_obj$winning_rho_num=3
explore_kpclr_obj


adot = anovadot(10,3)
k = kernelMatrix(kernel=adot, x=Xd)
max(k)
min(k)
kpca(k)

Xrf = rbind(explore_kpclr_obj$X_train, explore_kpclr_obj$X_validate)
yrf= c(explore_kpclr_obj$y_train, explore_kpclr_obj$y_validate)
table(explore_kpclr_obj$y_train)

rf = randomForest(explore_kpclr_obj$X_train, as.factor(explore_kpclr_obj$y_train), sampsize = c(70, 90))
rf
prf = predict(rf, explore_kpclr_obj$X_test)
table(explore_kpclr_obj$y_test, prf)
260*.67

kpca_obj = build_kpca_object(explore_kpclr_obj$X_train, kernel_type = "anova", c(10,3))
weights = weights_for_kpclr(explore_kpclr_obj$y_train, fn_to_fp_ratio = 5)
mod = kpclr(kpca_obj, explore_kpclr_obj$y_train, frac_var = .75, weights = weights)
preds = predict(mod, new_data = explore_kpclr_obj$X_test)
table(explore_kpclr_obj$y_test, preds > .5)

