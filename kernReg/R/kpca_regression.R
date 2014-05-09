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
  }
  
  err_mat = matrix(nrow = length(pc_seq), ncol = 3)
  colnames(err_mat) = c("Class 0", "Class 1", "Cost-Weighted")
  
  for(i in 1 : length(pc_seq)){
    mod = kpca_logistic_regression(y = ytrain, kpca_object = kpca_object, num_pcs = pc_seq[i] , weights = weights)
    preds = kernReg_predict(object = mod, new_data = xtest, training_data = xtrain)
    tab = table(factor(ytest, levels = c(0,1)), factor(as.numeric(preds > threshold), levels = c(0,1)))
    fp = tab[1,2]/sum(tab[1,]) ##FP
    fn = tab[2,1]/sum(tab[2,]) ##FN
    cost = fp * sum(tab[1,]) * fp_to_fn_cost + fn * sum(tab[2,])
    ##put cost here
    err_mat[i,] = c(fp, fn, cost)
    print(i)
  }

  plot(pc_seq, err_mat[,1], type = "o", col = "blue", ylim = c(0,1))
  points(pc_seq, err_mat[,2], type = "o", col = "red")
  #plot(pc_seq, err_mat[,3], type = "l")
  invisible(err_mat)
}

# kpca_object = kpca_obj
# xtrain = Split2Design
# xtest = Split3Design
# ytrain = Fail2
# ytest = Fail3
# fp_to_fn_cost = 2
# threshold = .67
# weights = NULL
