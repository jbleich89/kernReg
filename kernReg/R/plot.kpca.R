#' Plots the kernel matrix
#' 
#' Illustrates the kernel matrix as a heatmap. 
#' 
#' @param kpca_object 					The kpca object to plot
#' @param lower_triangular				If the kernel is symmetric (the usual case), setting this to \code{TRUE}
#' 										will only plot the lower triangle. Default is \code{TRUE}.
#' @param col.regions 					How to color the heatmap. The default is color but if you would like to produce a
#' 										grayscale images for publication, you can use something like \code{gray(100 : 0 / 100)}.
#' @param transform						An optional function to transform the entries of \code{K}. 
#' @param main							Title of the plot. Default is the description of the kernel.
#' @param ... 							Other parameters to pass to levelplot. 
#' 										Especially useful are \code{xlim} and \code{ylim} to look at a portion
#' 										of the matrix. Also, use the \code{at} to mimic limits
#' 										on the magnitudes of the entries of K. For instance \code{seq(1000, 2000, by = 20)}
#' 										will only graph entries between 1,000 and 2,000. Do not change the "by" parameter here,
#' 										20 seems to be a good choice. Standardize the \code{at} parameter allows better
#' 										apples : apples comparisons across the same kernel with different hyperparameters (e.g.
#' 										the radial basis kernel with different gamma values).
#' 
#' @author 								Adam Kapelner and Justin Bleich
#' @method plot kpca
#' @export
plot.kpca = function(kpca_object, lower_triangular = TRUE, transform = NULL, col.regions = rainbow(200, end = 0.78), main = NULL, ...){
	n = kpca_object$n
	mat = as.matrix(kpca_object$K[1 : n, 1 : n]) #the kernelMatrix class is resistant to change
	colnames(mat) = NULL
	rownames(mat) = NULL
	
	if (is.null(main)){ #default title is the description of the kernel 
		main = kernel_description(kpca_object)
	}
	
	if (!is.null(transform)){
		mat = transform(mat)
	}
	
	if (lower_triangular){ #make the plot lower triangular to emphasize it's a distance matrix and nuke the diagonal as well		
		for (i in 1 : n){
			for (j in 1 : n){
				if (i <= j){
					mat[i, j] = NA
				}
			}
		}		
	}

	levelplot(mat,
		xlab = "observation number", ylab = "observation number", 
		col.regions = col.regions, 
		main = main, 
		...)
}