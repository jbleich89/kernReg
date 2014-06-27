#' Run a linear regression using Kernel PCA
#' 
#' \code{kpcr} runs a linear regression on features created from an eigendecomposition
#' of a certain dimension of the kernelized data
#' 
#' @param kpca_object 		The object that contains the kernel, the kernelized data with its eigendecomposition 
#' @param y 				The response to be regressed on the features which are the principal components of the kernelized data
#' @param num_pcs 			The number of principal components to use for the regression (this or \code{frac_var} must be specified)
#' @param frac_var			Pick the number of principal components to use based on the fraction of variance to explain (this or \code{num_pcs} must be specified)
#' @return 					An lm object with the kpca_object embedded as well as the number of principal components used
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpclr}}
#' @references 				Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' 
#' @examples
#' \dontrun{
#' #pull the predictor matrix and response from the Boston Housing Data
#' data(Boston)
#' y = Boston$medv
#' Boston$medv = NULL
#' X = as.matrix(Boston)
#' #build a KPCA object using the anova kernel with hyperparameters sigma = 0.1 and d = 3 
#' kpca_obj = build_kpca_object(X, "anova", c(0.1, 3))
#' #build a KPCR model using 75% of the variance explained by the kernel matrix
#' kpcr_mod = kpcr(kpca_obj, y, frac_var = 0.75)
#' #print a summary of the model
#' kpcr_mod
#' }
#' @export
kpcr = function(kpca_object, y, num_pcs = NULL, frac_var = NULL){
	checkObjectType(kpca_object, "kpca_object", "kpca", "build_kpca_object")

	#ensure the user gave us either a number of PC's or a way to calculate a number of PC's from a fraction of variance explained
	if (is.null(num_pcs) && is.null(frac_var)){
		stop("Either \"num_pcs\" or \"frac_var\" must be specified.")
	} else if (is.null(num_pcs)){
		num_pcs = get_num_pcs_from_frac(kpca_object, frac_var)
	}	
	
    #regress the response on the data which has been kernelized 
    #then rotated onto a subset of the principal components
	X_kern_pca_red = as.data.frame(kpca_object$pc_mat[, 1 : num_pcs, drop = FALSE]) #creating a variable here makes the print statement nicer
    mod = lm(y ~ ., data = X_kern_pca_red)
  
    #return some other data to the user and mark this object as type "kpcr"
    mod$kpca_object = kpca_object
    mod$num_pcs = num_pcs
	mod$frac_var = frac_var
    class(mod) = c("kpcr", "lm") #kpcr is first in order to invoke it in the print statement
    mod
}