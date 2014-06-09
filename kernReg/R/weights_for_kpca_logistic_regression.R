#' Creates weights for kpca logistic regression
#' 
#' \code{weights_for_kpca_logistic_regression} will set individual weights
#' for each observation in the training data based on two things (1) the
#' ratio of 1's and 0's in the training set and (2) the user-set false-negative
#' to false-positive ratio.
#' 
#' @param y_train 			The responses for the training data - a binary vector of length \code{n}.
#' @param fn_to_fp_ratio 	The ratio of the "severity" of the false negative to the "severity" of the false positive (defaults to 1)
#' @return 					A vector of weights of length \code{n}.
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @export
weights_for_kpca_logistic_regression = function(y_train, fn_to_fp_ratio = 1){
	n_1 = sum(y_train == 1)
	n_0 = sum(y_train == 0)
	weight0 = (n_1 + n_0) / (n_0 * (fn_to_fp_ratio + 1))
	weight1 = (n_1 + n_0 - n_0 * weight0) / n_1
	ifelse(y_train == 1, weight1, weight0)
}
