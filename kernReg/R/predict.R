#' Predicts for new data
#' 
#' \code{predict.kpcr} predicts using the kernel PCA model for new data
#' 
#' @param object				The Kernel PCA linear model object used to predict 
#' @param new_data 				The new data the user wishes to predict
#' @param num_cores   			Number of cores for parallel prediction
#' @param ...					Other parameters to be passed to \code{predict.lm}
#' @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm}
#' 
#' @author 						Justin Bleich and Adam Kapelner
#' @seealso 					\code{\link{predict.lm}}
#' @method predict kpcr
#' 
#' @examples
#' \dontrun{
#' #pull the predictor matrix and response from the Boston Housing Data
#' data(Boston)
#' y = Boston$medv
#' Boston$medv = NULL
#' X = as.matrix(Boston)
#' #build a KPCA object using the anova kernel with hyperparameters gamma = 0.1 and d = 3 
#' kpca_obj = build_kpca_object(X, "anova", c(0.1, 3))
#' #build a kpcr model using 75% of the variance in the kernel matrix
#' kpcr_mod = kpcr(kpca_obj, y, frac_var = 0.75)
#' #forecast on the "new" data which here will just be the first 10 rows of the Boston Housing Data
#' x_star = X[1 : 10, ]
#' y_hat = predict(kpcr_mod, x_star)
#' }
#' @export
predict.kpcr = function(object, new_data, num_cores = 1, ...){
	checkObjectType(object, "kpcr_model_object", "kpcr", "kpca_regression")
	#procure the design matrix (i.e. the original data rotated onto the principal components)
	X_kernel_dim_red_names = colnames(object$model[, -1, drop = FALSE]) #kill the intercept
	#use the common code to predict
	kpca_predict_common(object, new_data, X_kernel_dim_red_names, num_cores = num_cores)
}

#' Predicts for new data
#' 
#' \code{predict.kpclr} predicts using the kernel PCA logistic model for new data
#' 
#' @param object				The Kernel PCA logistic model used to predict 
#' @param new_data 				The new data the user wishes to predict
#' @param type 					Which output to return to the user. Use "response" for predicted probability and "link" for a logit (see \code{predict.glm} for more information)
#' @param num_cores   			Number of cores for parallel prediction
#' @param ...					Other parameters to be passed to \code{predict.glm}
#' @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.glm}
#' 
#' @author 						Justin Bleich and Adam Kapelner
#' @seealso 					\code{\link{predict.glm}}
#' @method predict kpclr
#' 
#' @examples
#' \dontrun{
#' #pull the predictor matrix and dummify the response from the Boston Housing Data
#' data(Boston)
#' y = ifelse(Boston$medv > median(Boston$medv), 1, 0)
#' Boston$medv = NULL
#' X = as.matrix(Boston)
#' #build a KPCA object using the anova kernel with hyperparameters gamma = 0.1 and d = 3 
#' kpca_obj = build_kpca_object(X, "anova", c(0.1, 3))
#' #build a kpclr model using 75% of the variance in the kernel matrix and weights for 1:1 cost ratio
#' kpclr_mod = kpclr(kpca_obj, y, frac_var = 0.75, weights = weights_for_kpclr(y))
#' #forecast on the "new" data which here will just be the first 10 rows of the Boston Housing Data
#' x_star = X[1 : 10, ]
#' y_hat = predict(kpclr_mod, x_star)
#' }
#' @export
predict.kpclr = function(object, new_data, type = "response", num_cores = 1, ...){
	checkObjectType(object, "kpclr_model_object", "kpclr", "kpca_logistic_regression")
	#procure the design matrix (i.e. the original data rotated onto the principal components)
	X_kernel_dim_red_names = colnames(as.matrix(object$data))
	#use the common code to predict
	kpca_predict_common(object, new_data, X_kernel_dim_red_names, type, num_cores)
}

# Private method that does the heavy lifting for the predictions for new data 
# (see two functions above this for information). This can be significantly
# sped up if implemented in a lower level language like C++.
# 
# @param kpcr_model_object		The Kernel PCA logistic model used to predict 
# @param new_data 				The new data the user wishes to predict
# @param type 					Which output to return to the user. Use "response" for predicted probability and "link" for a logit (see \code{predict.glm} for more information)
# @param num_cores   			Number of cores for parallel prediction
# @param ...					Other parameters to be passed to \code{predict.lm} or \code{predict.glm}
# @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm} or \code{predict.glm}
# 
# @author 						Justin Bleich and Adam Kapelner
kpca_predict_common = function(kpcr_model_object, new_data, X_kernel_dim_red_names, type = "response", num_cores, ...){

  checkObjectType(new_data, "new_data", "matrix")
	#before we start predicting, we need to standardize new_data based on the average and sd of the training data
	n_star = nrow(new_data)
	new_data = (new_data - vec_to_mat(kpcr_model_object$kpca_object$X_j_averages, n_star)) / vec_to_mat(kpcr_model_object$kpca_object$X_j_standard_deviations, n_star)
	#for each row in new_data, we have to transform from x space to kernel space
	kernel = kpcr_model_object$kpca_object$kernel
	Xs = kpcr_model_object$kpca_object$Xs	
  
	if (num_cores > 1){
		#will work on windows -- not sure about unix/mac
		cluster = makeCluster(num_cores)
		registerDoParallel(cluster)
		
		i = NULL #this is only to shut up the --as-cran check NOTE that appears about i not having a binding
		k_vec_c_list = foreach(i = 1 : n_star) %dopar% {
			k_vec = K_vector(new_data[i,], Xs, kernel)
			K = kpcr_model_object$kpca_object$K
			k_vec_c = center_kernel_test_vec(k_vec, K)
			k_vec_c 
		}		
		stopCluster(cluster)
		#change to .combine = rbind above one day when I can test
		k_vecs_c = t(do.call(rbind, k_vec_c_list))
	} else {
		k_vecs = t(sapply(1 : n_star, function(s) K_vector(new_data[s, ], Xs, kernel)))
		#now we have to center the kernelized new data vectors
		K = kpcr_model_object$kpca_object$K
		k_vecs_c = sapply(1 : n_star, function(s) center_kernel_test_vec(k_vecs[s, ], K))		
	}
	
 	#now we have to take those kernelezied vectors and represent them in the basis of the eigenspace	
  	rotated_kvecs = (t(k_vecs_c) %*% kpcr_model_object$kpca_object$keigenvecs)
	#now truncate at the dimension we wish to represent them in the lower dimensional space based on the PC's we chose
	rotated_kvecs = as.matrix(rotated_kvecs[, 1 : kpcr_model_object$num_pcs, drop = FALSE])

	#if the inputted new_data that was different in size than the original data, throw an error
	if (length(X_kernel_dim_red_names) != ncol(rotated_kvecs)){
		stop("Need same number of columns in newdata as original data.")
	} 
	
	#make sure the new columns get the correct names so predict.lm can function properly
	colnames(rotated_kvecs) = X_kernel_dim_red_names
	
	#now predict using glm for logistic regression and lm for continuous regression
	if (is(kpcr_model_object, "kpclr")){
		predict.glm(kpcr_model_object, newdata = data.frame(rotated_kvecs), type = type, ...)
	} else {
		predict.lm(kpcr_model_object, newdata = data.frame(rotated_kvecs), ...)
	}
}


# Private method which kernelizes a new vector
# 
# @param xstar		The vector to be kernelized 
# @param X 			The original data
# @param kernel 	The kernel to use
# @return 			The kernelized vector
# 
# @author 			Justin Bleich and Adam Kapelner
K_vector = function(xstar, X, kernel){
	checkObjectType(X, "X", "matrix")
	#create the vector by running the function K on every observation in the training data with the new vector
	sapply(1 : nrow(X), function(s) kernel(xstar, X[s, ]))
}


# Private method which centers a new kernelized vector relative to the full K matrix
# 
# @param k_vec		The vector to be centered 
# @param K 			The full K matrix
# @return			The centered vector 
# 
# @author 			Justin Bleich and Adam Kapelner
center_kernel_test_vec = function(k_vec, K){
	n = length(k_vec)
	t(k_vec) - colSums(K) / n  - rep(sum(k_vec) / n , n) + sum(K) / n^2
}