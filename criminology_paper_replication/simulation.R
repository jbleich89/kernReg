library(kernReg)

##generate x's
set.seed(10)
nsim = 750
x1 = runif(nsim)
x2 = runif(nsim)

##generate y
pow = 4
klass = numeric(nsim)

for(i in 1:nsim){
  if(x1[i] < 1/2 & x2[i]> 1/.5^pow*x1[i]^pow){
    klass[i]=0
  } else if(x1[i] < 1/2 & x2[i]<1/.5^pow*x1[i]^pow){
    klass[i]=1
  } else if(x1[i]>1/2 & x2[i]< 1-1/(.5^pow)*(1-x1[i])^pow){
    klass[i]=0
  } else{
    klass[i]=1
  }  
}
table(klass)
y = klass
# data = data.frame(y,x1,x2)
# train = data[1 : nsim/3, ]
# validate = data[(nsim/3 + 1) : 2 * nsim/3, ]
# test = data[(2 * nsim/3 + 1) : nsim, ]

X = cbind(x1, x2)

kernel_list = list()
kernel_list[[1]] = list(kernel_type = "anova", params = c(.1, 3))
kernel_list[[2]] = list(kernel_type = "anova", params = c(1, 3))
#kernel_list[[3]] = list(kernel_type = "anova", params = c(10, 3))
# kernel_list[[4]] = list(kernel_type = "anova", params = c(10, 3))

explore_kpclr_obj = explore_kpclr_models(X, y, kernel_list = kernel_list)
plot(explore_kpclr_obj, quantile_cwe_to_display = 0.99, plot_tile_cols = 2)
explore_kpclr_obj = eval_winning_lr_model_on_test_data(explore_kpclr_obj)

kpca_obj = build_kpca_object(X[1:500,], kernel_type = "anova", params = c(1,3))
reg_mod = kpclr(kpca_obj, y[1:500], frac_var = .8)
preds = predict(reg_mod, new_data = X[501:750, ])
table(y[501:750], preds > .5)
