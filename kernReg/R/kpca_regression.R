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


kpca_logistic_regression = function(y, kpca_object, num_pcs){
  if(class(kpca_object) != "kernel_pca") stop("Need a kernel pca object. Call kernel_pca().")
  X = kpca_object$pc_mat[, 1 : num_pcs]
  mod = glm(y ~ ., data = as.data.frame(X), family = "binomial")
  mod$ols = FALSE
  mod$kpca_object = kpca_object
  mod$num_pcs = num_pcs
  class(mod) = c("glm", "lm", "kernReg")
  mod
}
