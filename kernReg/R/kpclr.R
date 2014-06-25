#' Run a logistic regression using Kernel PCA
#' 
#' \code{kpclr} runs a logistic regression on features created from an eigendecomposition
#' of a certain dimension of the kernelized data
#' 
#' @param kpca_object 		The object that contains the kernel and the kernelized data with its eigendecomposition 
#' @param y 				The response to be regressed on the features which are the principal components of the kernelized data
#' @param num_pcs 			The number of principal components to use for the regression (this or \code{frac_var} must be specified)
#' @param frac_var			Pick the number of principal components to use based on the fraction of variance to explain (this or \code{num_pcs} must be specified)
#' @param weights			Weights to be used on each observation in a weighted generalized least squares implementation. If not specified (default), uniform weights are used
#' @param family			The family parameter to be passed to the glm function. Default is "binomial." Note that with this default, AIC calculations are not possible.
#' 
#' @return 					An lm object with the kpca_object embedded as well as the number of principal components used
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpca_regression}}
#' @references 				Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
kpclr = function(kpca_object, y, num_pcs = NULL, frac_var = NULL, weights = NULL, family = "binomial"){
	checkObjectType(kpca_object, "kpca_object", "kpca", "build_kpca_object")
	
	#ensure the user gave us either a number of PC's or a way to calculate a number of PC's from a fraction of variance explained
	if (is.null(num_pcs) && is.null(frac_var)){
		stop("Either \"num_pcs\" or \"frac_var\" must be specified.")
	} else if (is.null(num_pcs)){
		num_pcs = get_num_pcs_from_frac(kpca_object, frac_var)
	}
	
	#regress the response on the data which has been kernelized 
	#then rotated onto a subset of the principal components. Use glm's binomial
	#method for a vanilla logistic regression implementation
	X_kern_pca_red = as.data.frame(kpca_object$pc_mat[, 1 : num_pcs, drop = FALSE]) #creating a variable here makes the print statement nicer
	mod = glm(y ~ ., data = X_kern_pca_red, family = family, weights = weights) #we use quasibinomial to avoid the non-integer successes warning
	
	#return some other data to the user and mark this object as type "kpclr" et al.
	mod$kpca_object = kpca_object
	mod$num_pcs = num_pcs
	mod$frac_var = frac_var
	mod$weights = weights
	class(mod) = c("kpclr", "kpcr", "glm", "lm") #kpclr is first in order to invoke it in the predict and print methods
	mod
}