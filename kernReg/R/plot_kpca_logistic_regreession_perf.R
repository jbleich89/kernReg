
#' Performance plot by %PC's of a Kernel PCA Logistic Regression
#' 
#' Plots the performance in terms of the false negative and false positive error of a kernel PCA
#' logistic regression model at various proportions of principal components used. To plot the 
#' performance, test data not seen during constrcution used to build the kernel PCA logistic 
#' regression model is used.
#'  
#' @param kpca_logistic_regression_object	The kernel PCA logistic regression model 
#' @param ytrain							The responses for the original training data
#' @param ytest 							The responses for the new testing data
#' @param xtest 							The features for the new testing data
#' @param var_seq 							A collection of proportions of the number of PC's to use when plotting performance
#' @param threshold 						The threshold of the probability estimate after which to call the estimate a "1"
#' @param weights 							A weight vector to use for the original training data
#' @return									A matrix of error rates for each proportion of the number of PC's in \code{var_seq} 
#' 
#' @author 									Justin Bleich and Adam Kapelner
#' @export
plot_kpca_logistic_regreession_perf = function(kpca_logistic_regression_object, ytrain, ytest, xtest, var_seq = seq(.05, .90, by = .05), threshold = .5, weights = NULL){
	checkObjectType(kpca_logistic_regression_object, "kpca_logistic_regression_object", "kernLogReg", "kpca_logistic_regression")
	
	err_mat = matrix(nrow = length(var_seq), ncol = 2)
	colnames(err_mat) = c("Class 0", "Class 1")#, "Cost-Weighted")
	
	for (i in 1 : length(var_seq)){
		mod = kpca_logistic_regression(y = ytrain, kpca_object = kpca_object, num_pcs = get_num_pcs(kpca_object, var_seq[i]) , weights = weights)
		preds = kernReg_predict(object = mod, new_data = xtest, training_data = xtrain)
		tab = table(factor(ytest, levels = c(0,1)), factor(as.numeric(preds > threshold), levels = c(0,1)))
		fp = tab[1,2]/sum(tab[1,]) ##FP
		fn = tab[2,1]/sum(tab[2,]) ##FN
		#cost = fp * sum(tab[1,]) * fp_to_fn_cost + fn * sum(tab[2,])
		#cost = 2 * tab[2,2]*tab[1,2]/sum(tab[,2])^2 + tab[1,1]*tab[2,1]/sum(tab[,1])^2
		#cost = 2 * tab[1,1]*tab[1,2]/sum(tab[1,])^2 + tab[2,1]*tab[2,2]/sum(tab[2,])^2
		#cost = tab[1,2]/tab[2,1]
		##put cost here
		err_mat[i,] = c(fp, fn)#, cost)
		print(i)
	}
	
	
	plot(var_seq, err_mat[,1], type = "l", col = "blue", ylim = c(0,1), xlab = "Variance of Kernel Matrix", ylab = "Class Error")
	points(var_seq, err_mat[,2], type = "l", col = "red")
	legend("topright", legend = c("No Fail", "Fail"), col = c("blue", "red"), lty = 1)
	invisible(err_mat)
}

