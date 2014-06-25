##DOE Example
setwd("C:/Users/jbleich/Dropbox/Research_Crim/DOE")
load("DOE_vars_list_updated.Rdata")
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
y = ifelse(y == "Graduate", 1, 0)

Xd = as.matrix(dummify_data(X))
dim(Xd)
length(y)
summary(Xd)
colnames(Xd)

kernel_list = list()
# kernel_list[[1]] = list(kernel_type = "anova", params = c(.1, 3))
kernel_list[[1]] = list(kernel_type = "anova", params = c(1, 3))
kernel_list[[2]] = list(kernel_type = "anova", params = c(10, 3))
#kernel_list[[3]] = list(kernel_type = "anova", params = c(100, 3))

explore_kpclr_obj = explore_kpclr_models(Xd, y, kernel_list = kernel_list, fn_cost = 5, fp_cost = 1)
plot(explore_kpclr_obj, quantile_cwe_to_display = 0.99, plot_tile_cols = 2)
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)

adot = anovadot(10,3)
k = kernelMatrix(kernel=adot, x=Xd)
max(k)
min(k)
kpca(k)
