#' Performance plot by \%PC's of a Kernel PCA Logistic Regression
#' 
#' Plots the performance in terms of the false negative and false positive error of a kernel PCA
#' logistic regression model at various proportions of principal components used. To plot the 
#' performance, test data not seen during constrcution used to build the kernel PCA logistic 
#' regression model is used.
#'  
#' @param kpca_object						The kernel PCA logistic regression model 
#' @param ytrain							The responses for the original training data
#' @param ytest 							The responses for the new testing data
#' @param xtest 							The features for the new testing data
#' @param var_seq 							A collection of proportions of the variance explained of the kernel matrix to use when plotting performance
#' @param threshold 						The threshold of the probability estimate after which to call the estimate a "1"
#' @param weights 							A weight vector to use for the original training data
#' @param ...								Other parameters to be passed to the plot function
#' @return									A matrix of error rates for each proportion of the number of PC's in \code{var_seq} 
#' 
#' @author 									Justin Bleich and Adam Kapelner
#' @references 								Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
plot_kpca_logistic_regression_perf = function(kpca_object, ytrain, ytest, xtest, var_seq = seq(.05, .90, by = .05), threshold = .5, weights = NULL, ...){
	checkObjectType(kpca_object, "kpca_object", "kpca", "build_kpca_object")
	
	err_mat = matrix(nrow = length(var_seq), ncol = 2)
	colnames(err_mat) = c("Class 0", "Class 1")#, "Cost-Weighted")
	
	for (i in 1 : length(var_seq)){
		#build the model with the % variance explained of interest and let the user know where we are
		mod = kpca_logistic_regression(y = ytrain, kpca_object = kpca_object, num_pcs = get_num_pcs_from_frac(kpca_object, var_seq[i]) , weights = weights)
		cat("calculating errors using a model with ", (var_seq[i] * 100), "% variance of the kernel\n", sep = "")
		
		#predict using the model on the test data (this takes the longest
		preds = predict(mod, xtest, ...) #by default, this returns probability estimates which is what we want
		#now tabulate, calculate and record the errors
		tab = table(factor(ytest, levels = c(0,1)), factor(as.numeric(preds > threshold), levels = c(0,1)))
		fp = tab[1,2]/sum(tab[1,]) ##FP
		fn = tab[2,1]/sum(tab[2,]) ##FN
		#cost = fp * sum(tab[1,]) * fp_to_fn_cost + fn * sum(tab[2,])
		#cost = 2 * tab[2,2]*tab[1,2]/sum(tab[,2])^2 + tab[1,1]*tab[2,1]/sum(tab[,1])^2
		#cost = 2 * tab[1,1]*tab[1,2]/sum(tab[1,])^2 + tab[2,1]*tab[2,2]/sum(tab[2,])^2
		#cost = tab[1,2]/tab[2,1]
		##put cost here
		err_mat[i,] = c(fp, fn)#, cost)		
	}
	
	#now plot both types of errors as a function of variance explained
	plot(var_seq, err_mat[,1], type = "l", col = "blue", ylim = c(0, 1), xlab = "Variance of Kernel Matrix", ylab = "Class Error", ...)
	points(var_seq, err_mat[,2], type = "l", col = "red")
	legend("topright", legend = c("No Fail", "Fail"), col = c("blue", "red"), lty = 1)
	invisible(err_mat)
}

