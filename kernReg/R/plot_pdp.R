#' Plots a PDP for a kernel regression
#' 
#' \code{plot_pdp} plots a partial dependence plot (PDP; Friedman, 2001) for one of the predictors. The function
#' allows the PDP to be plotted to arbitrary accuracy and allows parameters to be passed for customization 
#' of the plotting. This function relies on package \pkg{ICEbox}.
#' 
#' @param kpca_model 		A linear or logistic kernel PCA regression model
#' @param predictor 		The predictor to build a PDP for. It can be the number of the column or the column's name
#' @param type 				The type argument that gets passed to the \code{kpca_predict} function
#' @param frac_to_build 	The fraction of the design matrix to construct the PDP from.
#' @param ... 				Other parameters to pass to the \code{plot} function
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{predict.kpcr}}, \code{\link{predict.kpclr}}, \code{\link{plot}}
#' @references 				Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
plot_pdp = function(kpca_model, predictor, type = "link", frac_to_build = 1, ...){
	checkObjectType(kpca_model, "kpca_model", c("kpcr", "kpclr"), c("kpcr", "kpclr"))
	
	kernel_pca_model_ice = ice(kpca_model, kpca_model$kpca_object$Xs, predictor = predictor, 
			predictfcn = function(object, newdata){predict(object, newdata, type = type)},
			frac_to_build = frac_to_build)	
	n = kpca_model$kpca_object$n
	plot(kernel_pca_model_ice, 
			x_quantile = F, 
			plot_pdp = TRUE, 
			frac_to_plot = 0.1, 
			plot_orig_pts_preds = FALSE, 
			colorvec = rgb(rep(1, n), rep(1, n), rep(1, n)), #white for all non-PDP lines (hack, but okay)
			...)
}