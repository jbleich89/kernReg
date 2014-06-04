#' Plots a PDP for a kernel regression
#' 
#' \code{plot_pdp} plots a partial dependence plot (PDP; Friedman, 2001) for one of the predictors. The function
#' allows the PDP to be plotted to arbitrary accuracy and allows parameters to be passed for customization 
#' of the plotting
#' 
#' @param kpca_model 		A linear or logistic kernel PCA regression model
#' @param X 				The original design matrix for the model
#' @param predictor 		The predictor to build a PDP for. It can be the number of the column or the column's name
#' @param type 				The type argument that gets passed to the \code{kpca_predict} function
#' @param frac_to_build 	The fraction of the design matrix to construct the PDP from.
#' @param ... 				Other parameters to pass to the \code{plot} function
#' 
#' @author Justin Bleich and Adam Kapelner
#' @seealso \code{\link{kpca_predict}}, \link{\code{plot}}
#' @export
plot_pdp = function(kpca_model, X, predictor, type = "link", frac_to_build = 1, ...){
	checkObjectType(kpca_model, "kpca_model", "kernReg", c("kpca_regression", "kpca_logistic_regression"))
	
	kernel_pca_model_ice = ice(kpca_model, X, predictor = predictor, 
			predictfcn = function(object, newdata){kpca_predict(object, newdata, X, type = type)},
			frac_to_build = frac_to_build)	
	plot(kernel_pca_model_ice, 
			x_quantile = F, 
			plot_pdp = TRUE, 
			frac_to_plot = 0.1, 
			plot_orig_pts_preds = FALSE, 
			colorvec = rgb(rep(1, nrow(X)), rep(1, nrow(X)), rep(1, nrow(X))), #white for all non-PDP lines (hack, but okay)
			...)
}