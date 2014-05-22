kernReg_predict = function(object, new_data, training_data, type = "response"){
  if(!any(class(object) == "kernReg")) stop("Need a kernReg object.")
  X = matrix(NA)
  
  if (object$ols){
    X = object$model[, -1, drop = FALSE]
  } else {
    X = as.matrix(object$data)
  }

  k_vecs = t(sapply(1 : nrow(new_data), function(s) K_vector(xstar = new_data[s, ], X = training_data, fun = object$kpca_object$K_object$fun, params = object$kpca_object$K_object$params)))
  k_vecs_c = sapply(1 : nrow(new_data), function(s) center_kernel_test_vec(k_vec = k_vecs[s,], K = object$kpca_object$K_object$K))
  rotated_kvecs = (t(k_vecs_c) %*% object$kpca_object$eigenvecs)[ , 1 : object$num_pcs, drop = F ]
  rotated_kvecs = as.matrix(rotated_kvecs)
  
  if(ncol(X)!= ncol(rotated_kvecs)) stop("Need same number of columns in newdata as original data.")
  
  colnames(rotated_kvecs) = colnames(X)
  
  if (object$ols){
    preds = predict.lm(object, newdata = data.frame(rotated_kvecs))
  } else {
    preds = predict.glm(object, newdata = data.frame(rotated_kvecs), type = type)
  }
  preds
}

plot_pdp = function(kernel_pca_model, X, predictor, type = "link", frac_to_build = 1, ...){
	kernel_pca_model_ice = ice(kernel_pca_model, X, predictor = predictor, 
			predictfcn = function(object, newdata){kernReg_predict(object, newdata, X, type = type)},
			frac_to_build = frac_to_build)	
	plot(kernel_pca_model_ice, 
			x_quantile = F, 
			plot_pdp = TRUE, 
			frac_to_plot = 0.1, 
			plot_orig_pts_preds = FALSE, 
			colorvec = rgb(rep(1, nrow(X)), rep(1, nrow(X)), rep(1, nrow(X))),
			...)
}
