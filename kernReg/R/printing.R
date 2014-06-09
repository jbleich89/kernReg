#' Prints a summary of a kpca object
#' 
#' @param x			The kpca object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print kpca
#' @export
print.kpca = function(x, ...){
	parameters = names(kpar(x$kernel))
	n = nrow(x$X)
	cat("Kernel PCA: ", n," x ", n, " matrix built with the ", kernel_description(x), "\n", sep = "")
}

#private method which generates a description of this kernel as a string
kernel_description = function(kpca_object){
	kernel_name = gsub("kernel", "", class(kpca_object$kernel)[1])
	parameters = names(kpar(kpca_object$kernel))
	vals = paste(kpar(kpca_object$kernel))
	
	if (is.null(parameters)){
		paste("\"", kernel_name, "\" kernel\n(no params)", sep = "")
	} else {
		paste("\"", kernel_name, "\" kernel w/params\n", 
			paste(parameters, vals, sep = " = ", collapse = ", "), sep = "")		
	}
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

#' Prints a summary of a kernLogReg object
#' 
#' @param x			The kernLogReg object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print kernLogReg
#' @export
print.kernLogReg = function(x, ...){
	print(x$kpca_object)
	if (is.null(x$frac_var)){
		cat("using ", x$num_pcs, " of the ", nrow(x$kpca_object$X), " possible principal components:\n ", sep = "")
	} else {
		cat("using ", x$frac_var * 100, "% of the kernelized design variance, i.e ", x$num_pcs, " of the ", nrow(x$kpca_object$X), " possible principal components:\n", sep = "")
	}
	
	print(summary.glm(x))
}

#' Prints a summary of a kernLogReg object
#' 
#' @param object		The kernLogReg object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary kernLogReg
#' @export
summary.kernLogReg = function(object, ...){
	print(object, ...)
}

#' Prints a summary of a kernReg object
#' 
#' @param x			The kernReg object to be summarized in the console
#' @param ...		Other parameters to pass to the default print function
#' 
#' @author 			Justin Bleich and Adam Kapelner
#' @method print kernReg
#' @export
print.kernReg = function(x, ...){
	print(x$kpca_object)
	if (is.null(x$frac_var)){
		cat("using ", x$num_pcs, " of the ", nrow(x$kpca_object$X), " possible principal components:\n ", sep = "")
	} else {
		cat("using ", x$frac_var * 100, "% of the kernelized design variance, i.e ", x$num_pcs, " of the ", nrow(x$kpca_object$X), " possible principal components:\n", sep = "")
	}
	print(summary.lm(x))		
}

#' Prints a summary of a kernReg object
#' 
#' @param object		The kernReg object to be summarized in the console
#' @param ...			Other parameters to pass to the default summary function
#' 
#' @author 				Justin Bleich and Adam Kapelner
#' @method summary kernReg
#' @export
summary.kernReg = function(object, ...){
	print(object, ...)
}