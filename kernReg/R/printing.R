print.kpca = function(x, ...){
	kernel_name = gsub("kernel", "", class(x$kernel)[1])
	parameters = names(kpar(x$kernel))
	vals = paste(kpar(x$kernel))
	n = nrow(x$X)
	
	if (is.null(parameters)){
		cat("Kernel PCA: ", n," x ", n, " matrix built with kernel \"", kernel_name, "\"\n", sep = "")
	} else {
		cat("Kernel PCA: ", n," x ", n, " matrix built with the \"", kernel_name, "\" kernel with parameters ", 
			paste(parameters, vals, sep = " = ", collapse = ", "), "\n", sep = "")		
	}

}

summary.kpca = function(object, ...){
	print(object, ...)
}

print.kernLogReg = function(x, ...){
	print(x$kpca_object)
	if (is.null(x$frac_var)){
		cat("using ", x$num_pcs, " of the ", nrow(x$kpca_object$X), " possible principal components:\n ", sep = "")
	} else {
		cat("using ", x$frac_var * 100, "% of the kernelized design variance, i.e ", x$num_pcs, " of the ", ncol(x$kpca_object$X), " possible principal components:\n", sep = "")
	}
	
	print(summary.glm(x))
}

summary.kernLogReg = function(object, ...){
	print(object, ...)
}


print.kernReg = function(x, ...){
	print(x$kpca_object)
	if (is.null(x$frac_var)){
		cat("using ", x$num_pcs, " of the ", nrow(x$kpca_object$X), " possible principal components:\n ", sep = "")
	} else {
		cat("using ", x$frac_var * 100, "% of the kernelized design variance, i.e ", x$num_pcs, " of the ", ncol(x$kpca_object$X), " possible principal components:\n", sep = "")
	}
	print(summary.lm(x))		
}

summary.kernReg = function(object, ...){
	print(object, ...)
}