kernReg_predict = function(object, new_data, training_data){
  if(!any(class(object)== "kernReg")) stop("Need a kernReg object.")
  X = matrix(NA)
  if(object$ols == T){
    X = object$model[ ,-1, drop = F]
  }else{
    X = as.matrix(object$data)
  }

  k_vecs = t(sapply(1 : nrow(new_data), function(s) K_vector(xstar = new_data[s,], X = training_data, fun = object$kpca_object$K_object$fun, params = object$kpca_object$K_object$params)))
  k_vecs_c = sapply(1 : nrow(new_data), function(s) center_kernel_test_vec(k_vec = k_vecs[s,], K = object$kpca_object$K_object$K))
  rotated_kvecs = (t(k_vecs_c) %*% object$kpca_object$eigenvecs)[ , 1 : object$num_pcs, drop = F ]
  rotated_kvecs = as.matrix(rotated_kvecs)
  if(ncol(X)!= ncol(rotated_kvecs)) stop("Need same number of columns in newdata as original data.")
  colnames(rotated_kvecs) = colnames(X)
 
  
  if(object$ols == T){
    preds = predict.lm(object, newdata = data.frame(rotated_kvecs))
  }else{
    preds = predict.glm(object, newdata = data.frame(rotated_kvecs), type = "response")
  }
  preds
}