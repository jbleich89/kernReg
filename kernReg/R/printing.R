print.kernel_matrix = function(x, ...){
	cat("Kernel Matrix built with kernel:", "<KERNEL>", "parameters:", x$params, "\n")
}

summary.kernel_matrix = function(object, ...){
	print(object, ...)
}

print.kernel_pca = function(x, ...){
	cat("Kernel PCA, Matrix built with kernel:", "<KERNEL>", "parameters:", x$K_object$params, "\n")
}

summary.kernel_pca = function(object, ...){
	print(object, ...)
}