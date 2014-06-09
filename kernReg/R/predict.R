#' Predicts for new data
#' 
#' \code{predict.kernReg} predicts using the kernel PCA model for new data
#' 
#' @param object				The Kernel PCA linear model object used to predict 
#' @param new_data 				The new data the user wishes to predict
#' @param ...					Other parameters to be passed to \code{predict.lm}
#' @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm}
#' 
#' @author 						Justin Bleich and Adam Kapelner
#' @seealso 					\code{predict.lm}
#' @method predict kernReg
#' @export
predict.kernReg = function(object, new_data, ...){
	checkObjectType(object, "kpca_model_object", "kernReg", "kpca_regression")
	#procure the design matrix (i.e. the original data rotated onto the principal components)
	X_kernel_dim_red_names = colnames(object$model[, -1, drop = FALSE]) #kill the intercept
	#use the common code to predict
	kpca_predict_common(object, new_data, X_kernel_dim_red_names)
}

#' Predicts for new data
#' 
#' \code{predict.kernLogReg} predicts using the kernel PCA logistic model for new data
#' 
#' @param object				The Kernel PCA logistic model used to predict 
#' @param new_data 				The new data the user wishes to predict
#' @param type 					Which output to return to the user. Use "response" for predicted probability and "link" for a logit (see \code{predict.glm} for more information)
#' @param ...					Other parameters to be passed to \code{predict.glm}
#' @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.glm}
#' 
#' @author 						Justin Bleich and Adam Kapelner
#' @seealso 					\code{predict.glm}
#' @method predict kernLogReg
#' @export
predict.kernLogReg = function(object, new_data, type = "response", ...){
	checkObjectType(object, "kpca_model_object", "kernLogReg", "kpca_logistic_regression")
	#procure the design matrix (i.e. the original data rotated onto the principal components)
	X_kernel_dim_red_names = colnames(as.matrix(object$data))
	#use the common code to predict
	kpca_predict_common(object, new_data, X_kernel_dim_red_names, type)
}

# Private method that does the heavy lifting for the predictions for new data 
# (see two functions above this for information)
# 
# @param kpca_model_object		The Kernel PCA logistic model used to predict 
# @param new_data 				The new data the user wishes to predict
# @param type 					Which output to return to the user. Use "response" for predicted probability and "link" for a logit (see \code{predict.glm} for more information)
# @param ...					Other parameters to be passed to \code{predict.lm} or \code{predict.glm}
# @return						A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm} or \code{predict.glm}
# 
# @author 						Justin Bleich and Adam Kapelner
kpca_predict_common = function(kpca_model_object, new_data, X_kernel_dim_red_names, type = "response", ...){
	#for each row in new_data, we have to transform from x space to kernel space
	kernel = kpca_model_object$kpca_object$kernel
	X = kpca_model_object$kpca_object$X
	k_vecs = t(sapply(1 : nrow(new_data), function(s) K_vector(new_data[s, ], X, kernel)))
	#now we have to center the kernelized new data vectors
	K = kpca_model_object$kpca_object$K
	k_vecs_c = sapply(1 : nrow(new_data), function(s) center_kernel_test_vec(k_vecs[s, ], K))
	#now we have to take those kernelezied vectors and represent them in the basis of the eigenspace
	rotated_kvecs = (t(k_vecs_c) %*% kpca_model_object$kpca_object$keigenvecs)
	#now truncate at the dimension we wish to represent them in the lower dimensional space based on the PC's we chose
	rotated_kvecs = as.matrix(rotated_kvecs[, 1 : kpca_model_object$num_pcs, drop = FALSE])

	#if the inputted new_data that was different in size than the original data, throw an error
	if (length(X_kernel_dim_red_names) != ncol(rotated_kvecs)){
		stop("Need same number of columns in newdata as original data.")
	} 
	
	#make sure the new columns get the correct names so predict.lm can function properly
	colnames(rotated_kvecs) = X_kernel_dim_red_names
	
	#now predict using glm for logistic regression and lm for continuous regression
	if (is(kpca_model_object, "kernLogReg")){
		predict.glm(kpca_model_object, newdata = data.frame(rotated_kvecs), type = type, ...)
	} else {
		predict.lm(kpca_model_object, newdata = data.frame(rotated_kvecs), ...)
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
	m = length(k_vec)
	t(k_vec) - colSums(K) / m  - rep(sum(k_vec) / m , m) + sum(K) / m^2
}