#' Searches Kernel Space
#' 
#' Plots the performance in terms of the false negative and false positive error of a kernel PCA
#' logistic regression model at various proportions of principal components used over many provided
#' kernel models.
#'  
#' @param kernel_list						A list of kernels to assess performance of over a variety of PC's by \% explained. 
#' 											Each element of this list is a list itself with keys "kernel_type" and "params."  
#' @param xtrain							The training data
#' @param ytrain							The responses for the original training data
#' @param ytest 							The responses for the new testing data
#' @param xtest 							The features for the new testing data
#' @param var_seq 							A collection of proportions of the variance explained of the kernel matrix to use when plotting performance
#' @param threshold 						The threshold of the probability estimate after which to call the estimate a "1"
#' @param weights 							A weight vector to use for the original training data
#' @param ...								Other parameters to be passed to the plot function
#' @return									A list where each kernel of interest is paired with a matrix of error rates for 
#' 											each proportion of the number of PC's in \code{var_seq} 
#' 
#' @author 									Justin Bleich and Adam Kapelner
#' @references 								Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @seealso 								\code{\link{plot_kpca_logistic_regression_perf}}
#' @export
kernel_finder_logistic_regression = function(kernel_list, xtrain, ytrain, xtest, ytest, var_seq = seq(.05, .95, by = .10), threshold = .5, weights = NULL, ...){
	checkObjectType(kernel_list, "kernel_list", "list", "list()")
	num_kernels = length(kernel_list)
	#set the plotting based on the number of kernels the user wishes to try
	tile_length = ceiling(sqrt(num_kernels))	
	par(mfrow = c(tile_length, tile_length))
	
	#construct all the kernels first so if there's an error, the function doesn't blow up
	all_kernels = list()
	cat("building all kernels...")	
	for (i in 1 : num_kernels){
		checkObjectType(kernel_list[[i]], paste("Element #", i, " of kernel_list", sep = ""), "list", "list()")
		#build a kpca object. The build function will check for all errors.
		all_kernels[[i]] = build_kpca_object(xtrain, kernel_list[[i]]$kernel_type, kernel_list[[i]]$params)		
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
		res = plot_kpca_logistic_regression_perf(kpca_object, ytrain = Fail2, ytest = Fail1, xtest = Split1Design, var_seq = var_seq, weights = weights, main = desc)
		#record the results with the key being information about the kernel
		kernel_results[[desc]] = res
	}
	kernel_results
}