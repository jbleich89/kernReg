#' Plots a PDP for a kernel regression
#' 
#' \code{plot_pdp} plots a partial dependence plot (PDP; Friedman, 2001) for one of the predictors. The function
#' allows the PDP to be plotted to arbitrary accuracy and allows parameters to be passed for customization 
#' of the plotting. This function relies on package \pkg{ICEbox}.
#' 
#' @param kpcr_or_kpclr_model 		A linear or logistic kernel PCA regression model
#' @param predictor 		The predictor to build a PDP for. It can be the number of the column or the column's name
#' @param type 				The type argument that gets passed to the \code{kpca_predict} function
#' @param frac_to_build 	The fraction of the design matrix to construct the PDP from.
#' @param ... 				Other parameters to pass to the \code{plot} function
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{predict.kpcr}}, \code{\link{predict.kpclr}}, \code{\link{plot}}
#' @references 				Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @examples
#' \dontrun{
#' #Note this is example is for regression, but it works the same for classification
#' #pull the predictor matrix and response from the Boston Housing Data
#' data(Boston)
#' y = Boston$medv
#' Boston$medv = NULL
#' X = as.matrix(Boston)
#' #build a KPCA object using the anova kernel with hyperparameters gamma = 0.1 and d = 3 
#' kpca_obj = build_kpca_object(X, "anova", c(0.1, 3))
#' #build a KPCR model using 75% of the variance explained by the kernel matrix
#' kpcr_mod = kpcr(kpca_obj, y, frac_var = 0.75)
#' #now we want to illustrate a Partial Dependence Plot for the 6th of the 13 predictors, 
#' #number of average rooms in the houses in the census tract. Since we don't need perfect resolution, 
#' #we can allow it to build on only half of the data. For better resolution later, we can set 
#' #frac_to_build to 1.
#' plot_pdp(kpcr_mod, 2, frac_to_build = 0.5)
#' }
#' @export
plot_pdp = function(kpcr_or_kpclr_model, predictor, type = "link", frac_to_build = 1, ...){
	checkObjectType(kpcr_or_kpclr_model, "kpca_model", c("kpcr", "kpclr"), c("kpcr", "kpclr"))
	
	kernel_pca_model_ice = ice(kpcr_or_kpclr_model, kpcr_or_kpclr_model$kpca_object$Xs, predictor = predictor, 
			predictfcn = function(object, newdata){predict(object, newdata, type = type)},
			frac_to_build = frac_to_build)	
	n = kpcr_or_kpclr_model$kpca_object$n
	plot(kernel_pca_model_ice, 
			x_quantile = F, 
			plot_pdp = TRUE, 
			frac_to_plot = 0.1, 
			plot_orig_pts_preds = FALSE, 
			colorvec = rgb(rep(1, n), rep(1, n), rep(1, n)), #white for all non-PDP lines (hack, but okay)
			...)
}