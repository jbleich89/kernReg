##Regression Methods

kpca_regression = function(y, kpca_object, num_pcs){
  if(class(kpca_object) != "kernel_pca") stop("Need a kernel pca object. Call kernel_pca().")
  X = kpca_object$pc_mat[, 1 : num_pcs]
  mod = lm(y ~ ., data = as.data.frame(X))
  mod$ols = TRUE
  mod$kpca_object = kpca_object
  mod$num_pcs = num_pcs
  class(mod) = c("lm", "kernReg")
  mod
}


kpca_logistic_regression = function(y, kpca_object, num_pcs, weights = NULL){
  if(class(kpca_object) != "kernel_pca") stop("Need a kernel pca object. Call kernel_pca().")
  X = kpca_object$pc_mat[, 1 : num_pcs]
  mod = glm(y ~ ., data = as.data.frame(X), family = "binomial", weights = weights)
  mod$ols = FALSE
  mod$kpca_object = kpca_object
  mod$num_pcs = num_pcs
  class(mod) = c("glm", "lm", "kernReg")
  mod
}

plot_perf = function(ytrain, xtrain, ytest, xtest, kpca_object, pc_seq = NULL, fp_to_fn_cost = 1, threshold = .5, weights = NULL){
  if(class(kpca_object) != "kernel_pca") stop("Need a kernel pca object. Call kernel_pca().")
  if(is.null(pc_seq)){
    num_pcs = get_num_pcs(kpca_object = kpca_object, frac_var_to_explain = .95)
    pc_seq = seq(1, num_pcs, by = 20)
    var_seq = seq(.05, .95, by = .05)
  }
  
  err_mat = matrix(nrow = length(var_seq), ncol = 3)
  colnames(err_mat) = c("Class 0", "Class 1", "Cost-Weighted")
  
  for(i in 1 : length(var_seq)){
    mod = kpca_logistic_regression(y = ytrain, kpca_object = kpca_object, num_pcs = get_num_pcs(kpca_object, var_seq[15]) , weights = weights)
    preds = kernReg_predict(object = mod, new_data = xtest, training_data = xtrain)
    tab = table(factor(ytest, levels = c(0,1)), factor(as.numeric(preds > threshold), levels = c(0,1)))
    fp = tab[1,2]/sum(tab[1,]) ##FP
    fn = tab[2,1]/sum(tab[2,]) ##FN
    #cost = fp * sum(tab[1,]) * fp_to_fn_cost + fn * sum(tab[2,])
    #cost = 2 * tab[2,2]*tab[1,2]/sum(tab[,2])^2 + tab[1,1]*tab[2,1]/sum(tab[,1])^2
    #cost = 2 * tab[1,1]*tab[1,2]/sum(tab[1,])^2 + tab[2,1]*tab[2,2]/sum(tab[2,])^2
    cost = tab[1,2]/tab[2,1]
    ##put cost here
    err_mat[i,] = c(fp, fn, cost)
    print(i)
  }

  plot(var_seq, err_mat[,1], type = "l", col = "blue", ylim = c(0,1), xlab = "Variance of Kernel Matrix", ylab = "Class Error")
  points(var_seq, err_mat[,2], type = "l", col = "red")
   legend("topright", legend = c("No Fail", "Fail"), col = c("blue", "red"), lty=1)
  windows()
  plot(var_seq, err_mat[,3], type = "l")
  invisible(err_mat)
}
err_mat

# Kobj = K_matrix(X = Split2Design, fun = k_anova_basis, params = k_params) ##build kernel object
# kpca_object = kernel_pca(K_object = Kobj) ## get kernel pca of object -- same call as kpca() essentiall
# xtrain = Split2Designs
# xtest = Split1Design
# ytrain = Fail2
#  ytest = Fail1
# fp_to_fn_cost = 2
# threshold = .67
# weights = NULL
# k_params = c(5,2)
