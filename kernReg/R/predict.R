#' Predicts for new data
#' 
#' \code{predict.kernReg} predicts using the kernel PCA model for new data
#' 
#' @param kpca_model_object		The Kernel PCA linear model used to predict 
#' @param new_data 				The new data the user wishes to predict
#' @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm}
#' 
#' @author 						Justin Bleich and Adam Kapelner
#' @seealso 					\link{\code{predict.lm}}
#' @export
predict.kernReg = function(kpca_model_object, new_data){
	checkObjectType(kpca_model_object, "kpca_model_object", "kernReg", "kpca_regression")
	#procure the design matrix (i.e. the original data rotated onto the principal components)
	X_kernel_dim_red_names = colnames(kpca_model_object$model[, -1, drop = FALSE]) #kill the intercept
	#use the common code to predict
	kpca_predict_common(kpca_model_object, new_data, X_kernel_dim_red_names)
}

#' Predicts for new data
#' 
#' \code{predict.kernLogReg} predicts using the kernel PCA logistic model for new data
#' 
#' @param kpca_model_object		The Kernel PCA logistic model used to predict 
#' @param new_data 				The new data the user wishes to predict
#' @param type 					Which output to return to the user. Use "response" for predicted probability and "link" for a logit (see \link{\code{predict.glm}} for more information)
#' @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm}
#' 
#' @author 						Justin Bleich and Adam Kapelner
#' @seealso 					\link{\code{predict.glm}}
#' @export
predict.kernLogReg = function(kpca_model_object, new_data, type = "response"){
	checkObjectType(kpca_model_object, "kpca_model_object", "kernLogReg", "kpca_logistic_regression")
	#procure the design matrix (i.e. the original data rotated onto the principal components)
	X_kernel_dim_red_names = colnames(as.matrix(kpca_model_object$data))
	#use the common code to predict
	kpca_predict_common(kpca_model_object, new_data, X_kernel_dim_red_names, type)
}

#' Predicts for new data (see two functions above this for information)
#' 
#' \code{kpca_predict_common} predicts using the kernel PCA model for new data for both linear and logistic regressison
kpca_predict_common = function(kpca_model_object, new_data, X_kernel_dim_red_names, type = "response"){
	#for each row in new_data, we have to transform from x space to kernel space
	kernel = kpca_model_object$kpca_object$kernel
	X = kpca_model_object$kpca_object$X
	k_vecs = t(sapply(1 : nrow(new_data), function(s) K_vector(new_data[s, ], X, kernel)))
	#now we have to center the kernelized new data vectors
	K = kpca_model_object$kpca_object$K
	k_vecs_c = sapply(1 : nrow(new_data), function(s) center_kernel_test_vec(k_vecs[s, ], K))
	#now we have to take those kernelezied vectors and represent them in the lower dimensional space based on the PC's we chose
	rotated_kvecs = (t(k_vecs_c) %*% kpca_model_object$kpca_object$keigenvecs)
	#now truncate at the dimension we wish
	rotated_kvecs = as.matrix(rotated_kvecs[, 1 : kpca_model_object$num_pcs, drop = FALSE])

	#if the inputted new_data that was different in size than the original data, throw an error
	if (length(X_kernel_dim_red_names) != ncol(rotated_kvecs)){
		stop("Need same number of columns in newdata as original data.")
	} 
	
	#make sure they get the correct names
	colnames(rotated_kvecs) = X_kernel_dim_red_names
	
	#now predict using glm for logistic regression and lm for continuous regression
	if (is(kpca_model_object, "kernLogReg")){
		predict.glm(kpca_model_object, newdata = data.frame(rotated_kvecs), type = type)
	} else {
		predict.lm(kpca_model_object, newdata = data.frame(rotated_kvecs))
	}
}


#' Kernelizes a new vector
#' 
#' @param xstar		The vector to be kernelized 
#' @param X 		The original data
#' @param kernel 	The kernel to use
#' @return 			The kernelized vector
#' 
#' @author 			Justin Bleich and Adam Kapelner
K_vector = function(xstar, X, kernel){
	checkObjectType(X, "X", "matrix")
	#create the vector by running the function K on every observation in the training data with the new vector
	sapply(1 : nrow(X), function(s) kernel(xstar, X[s, ]))
}


#' Centers a new kernelized vector relative to the full K matrix
#' 
#' @param k_vec		The vector to be centered 
#' @param K 		The full K matrix
#' @return			The centered vector 
#' 
#' @author 			Justin Bleich and Adam Kapelner
center_kernel_test_vec = function(k_vec, K){
	m = length(k_vec)
	t(k_vec) - colSums(K) / m  - rep(sum(k_vec) / m , m) + sum(K) / m^2
}