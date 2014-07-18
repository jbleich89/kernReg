#' Prints a summary of a kpca object
#' 
#' @param x			The kpca object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print kpca
#' @export
print.kpca = function(x, ...){
	cat("Kernel PCA: ", x$n," x ", x$n, " matrix built with the ", kernel_description(x), "\n", sep = "")
}

#' Prints a summary of a kpca object
#' 
#' @param object		The kpca object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary kpca
#' @export
summary.kpca = function(object, ...){
	print(object, ...)
}

#' Prints a summary of a kpclr object
#' 
#' @param x			The kpclr object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print kpclr
#' @export
print.kpclr = function(x, ...){
	print(x$kpca_object)
	if (is.null(x$frac_var)){
		cat("using ", x$num_pcs, " of the ", x$kpca_object$n, " possible principal components:\n ", sep = "")
	} else {
		cat("using ", x$frac_var * 100, "% of the kernelized design variance, i.e ", x$num_pcs, " of the ", x$kpca_object$n, " possible principal components:\n", sep = "")
	}
	
	print(summary.glm(x))
}

#' Prints a summary of a kpclr object
#' 
#' @param object		The kpclr object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary kpclr
#' @export
summary.kpclr = function(object, ...){
	print(object, ...)
}

#' Prints a summary of a kpcr object
#' 
#' @param x			The kpcr object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print kpcr
#' @export
print.kpcr = function(x, ...){
	print(x$kpca_object)
	if (is.null(x$frac_var)){
		cat("using ", x$num_pcs, " of the ", x$kpca_object$n, " possible principal components:\n ", sep = "")
	} else {
		cat("using ", x$frac_var * 100, "% of the kernelized design variance, i.e ", x$num_pcs, " of the ", x$kpca_object$n, " possible principal components:\n", sep = "")
	}
	print(summary.lm(x))		
}

#' Prints a summary of a kpcr object
#' 
#' @param object		The kpcr object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary kpcr
#' @export
summary.kpcr = function(object, ...){
	print(object, ...)
}

#' Prints a summary of a explore_kpclr object
#' 
#' @param x			The explore_kpclr object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print explore_kpclr
#' @export
print.explore_kpclr = function(x, ...){
	cat("Explore Logistic models ", x$num_kernels, " kernels, rhos = [", paste(x$rho_seq, collapse = ", ", sep = ""), "],\nn_train/n_validate/n_test = ", x$n_train, "/", x$n_validate, "/", x$n_test, "\n", sep = "")
	if (!is.null(x$winning_kernel_num)){
		cat("winning kernel/rho = ", x$winning_kernel_num, "/", x$winning_rho_num, "\n", sep = "")
	}
	if (!is.null(x$test_confusion)){
		cat("out-of-sample performance: confusion table:\n")
		print(x$test_confusion)
		cat("weighted cost:", round(x$test_weighted_cost, 2), "\nmisclassification error:", round(x$test_misclassification_error, 3), "\n")
	}
}

#' Prints a summary of a explore_kpclr object
#' 
#' @param object		The explore_kpclr object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary explore_kpclr
#' @export
summary.explore_kpclr = function(object, ...){
	print(object, ...)
}


#' Prints a summary of a explore_kpcr object
#' 
#' @param x			The \code{explore_kpcr} object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print explore_kpcr
#' @export
print.explore_kpcr = function(x, ...){
	cat("Explore Regression models ", x$num_kernels, " kernels, rhos = [", paste(x$rho_seq, collapse = ", ", sep = ""), "],\nn_train/n_validate/n_test = ", x$n_train, "/", x$n_validate, "/", x$n_test, "\n", sep = "")
	if (!is.null(x$winning_kernel_num)){
		cat("winning kernel/rho = ", x$winning_kernel_num, "/", x$winning_rho_num, "\n", sep = "")
	}
	if (!is.null(x$L2_err)){
		cat("out-of-sample performance: L2_err:", round(x$L2_err, 2), "\nrmse:", round(x$rmse, 3), "\nL1_err:", round(x$L1_err, 3), "\n")
	}
}

#' Prints a summary of a explore_kpcr object
#' 
#' @param object		The \code{explore_kpcr} object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary explore_kpcr
#' @export
summary.explore_kpcr = function(object, ...){
	print(object, ...)
}


#private method which generates a description of this kernel as a string
kernel_description = function(kpca_object){
	kernel_name = gsub("kernel", "", class(kpca_object$kernel)[1])
	parameters = names(kpar(kpca_object$kernel))
	parameters[parameters == "sigma"] = "gamma" #We've made the decision to rename "sigma" => "gamma" to emphasize the point that gamma = inverse of (2 * sigsq).
	vals = paste(kpar(kpca_object$kernel))
	
	if (is.null(parameters)){
		paste("\"", kernel_name, "\" kernel\n(no params)", sep = "")
	} else {
		paste("\"", kernel_name, "\" kernel w/params\n", 
				paste(parameters, vals, sep = " = ", collapse = ", "), sep = "")		
	}
}