#a set of the valid kernel types for the method build_kpca_object
VALID_KERNEL_TYPES = c("vanilla", "rbf", "poly", "tanh", "bessel", "laplace", "anova", "spline")
#a constant equal to numerical zero for our intents and purposes (deletes minor dimensions of eigenspace)
MIN_POSITIVE_EIGENVALUE = 1e-4

#' Builds a Kernel PCA Object
#' 
#' Based on the original design matrix (the "data"), build an object which houses information about
#' the data in a transformed / kernelized space based on a kernel of the user's choice.
#'  
#' @param X				The original design matrix 
#' @param kernel_type 	One of the valid kernel types: vanilla, rbf, poly, tanh, bessel, laplace, anova, spline. 
#' @param params 		A list of parameters specific to the kernel of the user's choice. Each kernel type has 
#' 						a required number of parameters that must be passed otherwise the function will throw
#' 						an error.
#' @return 				A list composed of the original data, the kernel, the matrix K, the non-zero eigenvalues and eigenvectors of K and K in the eigenbasis	
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @seealso 			See \code{\link[kernlab]{dots}} for more information.
#' @references 			Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
build_kpca_object = function(X, kernel_type, params = c()){
	checkObjectType(X, "X", "matrix")
	
	#ensure that the kernel_type is valid
  	if (!(kernel_type %in% VALID_KERNEL_TYPES)){
	  stop("The kernel type must be one of the following:", paste(VALID_KERNEL_TYPES, collapse = ", "))
  	}
	
	#now build the the correct kernel based on the kernel type. This is a giant switch statement
	if (kernel_type == "vanilla"){
		if (length(params) != 0){
			stop("vanilla kernels do not take parameters")
		}
		kernel = vanilladot()
	} else if (kernel_type == "rbf"){
		if (length(params) != 1){
			stop("gaussian radial basis function kernels require one parameter: gamma")
		}
		kernel = rbfdot(params[1])
	} else if (kernel_type == "poly"){
		if (length(params) != 3){
			stop("polynomial kernels require three parameters: degree, scale and offset in that order")
		}
		kernel = polydot(params[1], params[2])
	} else if (kernel_type == "tanh"){
		if (length(params) != 2){
			stop("hyperbolic tangent kernels require two parameters: scale and offset in that order")
		}
		kernel = tanhdot(params[1], params[2])
	} else if (kernel_type == "bessel"){
		if (length(params) != 3){
			stop("bessel kernels require three parameters: gamma, order and degree in that order")
		}
		kernel = besseldot(params[1], params[2], params[3])
	} else if (kernel_type == "laplace"){
		if (length(params) != 1){
			stop("laplace kernels require one parameter: gamma")
		}
		kernel = laplacedot(params[1])
	} else if (kernel_type == "anova"){
		if (length(params) != 2){
			stop("anova kernels require two parameters: gamma and degree in that order")
		}
		kernel = anovadot(params[1], params[2])
	} else if (kernel_type == "spline"){
		if (length(params) != 0){
			stop("spline kernels do not take parameters")
		}
		kernel = splinedot()
	}
	
	#now we have built the kernel, let's compute the kernel matrix
	K = kernelMatrix(kernel, X)
	#now let's center it
	Kc = center_kernel_matrix(K)
	#from the centered kernel matrix, we can compute
	keigen = eigen(Kc / nrow(Kc), symmetric = TRUE) #it is faster to specify symmetric as true
	#calculate the number of positive eigenvalues up to numerical stability
	num_pos_eigenvecs = sum(keigen$values > MIN_POSITIVE_EIGENVALUE)
	#now pull out the eigenvectors
	keigenvecs = t(t(keigen$vectors[, 1 : num_pos_eigenvecs]) / sqrt(keigen$values[1 : num_pos_eigenvecs]))
	#rotate the eigenvectors onto the data
	pc_mat = Kc %*% keigenvecs
	
	#let's pass back all information as a list
	obj = list(X = X, kernel = kernel, K = K, keigenvals = keigen$values[1 : num_pos_eigenvecs], keigenvecs = keigenvecs, pc_mat = pc_mat)
	class(obj) = "kpca"
	obj
}


# A private helper function which centers a kernel matrix
# 
# @param K		The kernel matrix to be centered 
# @return		The centered kernel matrix 
# 
# @author 		Justin Bleich and Adam Kapelner
center_kernel_matrix = function(K){
  m = dim(K)[1]
  t(t(K - colSums(K) / m) - rowSums(K) / m) + sum(K) / m^2
}


