##DOE Example
setwd("C:/Users/jbleich/Dropbox/Research_Crim/DOE")
load("fullDOE.Rdata")
library(kernReg)
library(randomForest)

kernel_list = list()
kernel_list[[1]] = list(kernel_type = "anova", params = c(1, 3))
kernel_list[[2]] = list(kernel_type = "anova", params = c(10, 3))

explore_kpclr_obj = explore_kpclr_models(Xd, y, kernel_list = kernel_list, fn_cost = 5, fp_cost = 1)
plot(explore_kpclr_obj, quantile_cwe_to_display = 0.99, plot_tile_cols = 2)
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)
explore_kpclr_obj

adot = anovadot(10,3)
k = kernelMatrix(kernel=adot, x=Xd)
max(k)
min(k)
kpca(k)
