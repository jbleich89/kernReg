#' Searches Kernel Space
#' 
#' Plots the performance in terms of the false negative and false positive error of a kernel PCA
#' logistic regression model at various proportions of principal components used over many provided
#' kernel models.
#'  
#' @param kernel_list						A list of kernels to assess performance of over a variety of PC's by \% explained. 
#' 											Each element of this list is a list itself with keys "kernel_type" and "params."  
#' @param X_train							The training data
#' @param y_train							The responses for the original training data
#' @param y_validate 						The responses for the new validation data
#' @param X_validate 						The features for the new validation data
#' @param var_seq 							A collection of proportions of the variance explained of the kernel matrix to use when plotting performance
#' @param threshold 						The threshold of the probability estimate after which to call the estimate a "1"
#' @param weights 							A weight vector to use for the original training data
#' @param tile_cols							The number of columns in the plot array.
#' @param ...								Other parameters to be passed to the plot function
#' @return									A list where each kernel of interest is paired with a matrix of error rates for 
#' 											each proportion of the number of PC's in \code{var_seq} 
#' 
#' @author 									Justin Bleich and Adam Kapelner
#' @references 								Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @seealso 								\code{\link{plot_kpca_logistic_regression_perf}}
#' @export
kernel_finder_logistic_regression = function(kernel_list, X_train, y_train, X_validate, y_validate, var_seq = seq(.05, .95, by = .10), threshold = .5, weights = NULL, tile_cols = 3, ...){
	checkObjectType(kernel_list, "kernel_list", "list", "list()")
	num_kernels = length(kernel_list)
	#set the plotting based on the number of kernels the user wishes to try
	tile_rows = ceiling(num_kernels / tile_cols)	
	par(mfrow = c(tile_rows, tile_cols))
	
	#construct all the kernels first so if there's an error, the function doesn't blow up
	all_kernels = list()
	cat("building all kernels...")	
	for (i in 1 : num_kernels){
		checkObjectType(kernel_list[[i]], paste("Element #", i, " of kernel_list", sep = ""), "list", "list()")
		#build a kpca object. The build function will check for all errors.
		all_kernels[[i]] = build_kpca_object(X_train, kernel_list[[i]]$kernel_type, kernel_list[[i]]$params)		
	}
	cat("done\n")
	
	#now iterate over all the kernels and plot the performance graph for each of them
	#for each, tabulate the results by kernel
	kernel_results = list()
	for (i in 1 : num_kernels){
		kpca_object = all_kernels[[i]]
		#Then let the user know this kernel is being checked for performance
		desc = kernel_description(kpca_object)
		cat(desc, "\n")		
		#now go ahead and make a plot
		res = plot_kpca_logistic_regression_perf(kpca_object, 
				y_train = y_train, 
				X_validate = X_validate, 
				y_validate = y_validate,				
				var_seq = var_seq, 
				weights = weights,
				legend = FALSE,
				main = desc)
		#record the results with the key being information about the kernel
		kernel_results[[desc]] = res
	}
	invisible(kernel_results)
}