library(kernReg)
library(MASS)
data(Boston)

y = Boston$medv
Boston$medv = NULL
X = as.matrix(Boston)

explore_kpcr_obj = explore_kpcr_models(X, y)

plot(explore_kpcr_obj, tile_cols = 4)

#we now let the automatic selector try to find a good kernel
explore_kpcr_obj = auto_select_best_kpcr_model(explore_kpcr_obj)
plot(explore_kpcr_obj, tile_cols = 2)
#the kernel the automatic selector found is not great because it did not factor in parsimony, so let's pick...
explore_kpcr_obj = set_desired_model(explore_kpcr_obj, 3, 7)
plot(explore_kpcr_obj, tile_cols = 2)

#now let's see how this kernel regression PC model does on the test data
explore_kpcr_obj = eval_winning_r_model_on_test_data(explore_kpcr_obj, use_validation_data = TRUE)
explore_kpcr_obj

#how does RF do?
library(randomForest)
rf_mod = randomForest(rbind(explore_kpcr_obj$X_train, explore_kpcr_obj$X_validate), c(explore_kpcr_obj$y_train, explore_kpcr_obj$y_validate))
y_hat = predict(rf_mod, explore_kpcr_obj$X_test)
L2 = sum((y_hat - explore_kpcr_obj$y_test)^2)
L2
rmse = sqrt(L2 / explore_kpcr_obj$n_test)
rmse
