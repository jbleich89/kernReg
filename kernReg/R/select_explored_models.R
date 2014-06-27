#' Auto-Select Best KPCR Model
#' 
#' Selects the best KPCR model based on finding the minimum sum of squared error on the validation data. The 
#' model can always be hand-selected using \code{\link{set_desired_model}}.
#' 
#' @param explore_kpcr_obj 		An object of type \code{explore_kpcr}.
#' @return 						An object of type \code{explore_kpcr} with information regarding the auto-
#' 								selected model.
#' 
#' @author 						Adam Kapelner and Justin Bleich
#' 
#' @examples
#' \dontrun{
#' #first create data
#' X = matrix(rnorm(300), ncol = 4)
#' y = rnorm(300)
#' #now explore kernel models using the default kernel list
#' explore_kpcr_obj = explore_kpcr_models(X, y)
#' #now we plot to see how the models built on the training data performed on the validation data
#' plot(explore_kpcr_obj)
#' #we are comfortable with allowing the computer to decide which model is best based on lowest SSE
#' explore_kpcr_obj = auto_select_best_kpcr_model(explore_kpcr_obj)
#' #re-plotting shows a blue line indicating the favored model
#' plot(explore_kpcr_obj)
#' }
#' @export
auto_select_best_kpcr_model = function(explore_kpcr_obj){
	winning = which(explore_kpcr_obj$sse_validation_results == min(explore_kpcr_obj$sse_validation_results), arr.ind = TRUE)
	explore_kpcr_obj$winning_kernel_num = winning[1]
	explore_kpcr_obj$winning_rho_num = winning[2]
	explore_kpcr_obj
}


#' Auto-Select Best KPCLR Model
#' 
#' Selects the best KPCLR model based based on finding the minimum cost-weighted error model that has a fn:fp ratio
#' within the bounds defined by \code{fp_max_cost}, \code{fn_min_cost}, \code{fp_min_cost} 
#' and \code{fn_max_cost}.  The model can always be hand-selected using \code{\link{set_desired_model}}.
#' 
#' @param explore_kpclr_obj		An object of type \code{explore_kpclr}.
#' @param fp_max_cost			The maximum cost of a false positve. Together with \code{fn_min_cost}, this will inform
#' 								the algorithm of the maximum cost ratio of fp:fn. If left to the default \code{NULL}, the maximum
#' 								cost ratio will be 25\% more than the desired cost ratio.
#' @param fn_min_cost			The minimum cost of a false negative. Together with \code{fp_max_cost}, this will inform
#' 								the algorithm of the maximum cost ratio of fp:fn. If left to the default \code{NULL}, the maximum
#' 								cost ratio will be 25\% more than the desired cost ratio. 
#' @param fp_min_cost			The minimum cost of a false positive. Together with \code{fn_max_cost}, this will inform
#' 								the algorithm of the minimum cost ratio of fp:fn. If left to the default \code{NULL}, the minimum
#' 								cost ratio will be 25\% less than the desired cost ratio. 
#' @param fn_max_cost			The maximum cost of a false negative. Together with \code{fp_min_cost}, this will inform
#' 								the algorithm of the minimum cost ratio of fp:fn. If left to the default \code{NULL}, the minimum
#' 								cost ratio will be 25\% less than the desired cost ratio. 
#' @return 						An object of type \code{explore_kpclr} with information regarding the auto-
#' 								selected model.
#' 
#' @author 						Adam Kapelner and Justin Bleich
#' 
#' @examples
#' \dontrun{
#' #first create classification data
#' X = matrix(rnorm(300), ncol = 4)
#' y = rbinom(300, 1, 0.5)
#' #now explore kernel models using the default kernel list
#' explore_kpclr_obj = explore_kpclr_models(X, y)
#' #now we plot to see how the models built on the training data performed on the validation data
#' plot(explore_kpclr_obj)
#' #we are comfortable with allowing the computer to decide which model is best based on 
#' #the minimum cost-weighted error model which falls wthin a default range of the error ratio. 
#' explore_kpclr_obj = auto_select_best_kpclr_model(explore_kpclr_obj)
#' #re-plotting shows a blue line indicating the favored model
#' plot(explore_kpclr_obj)
#' }
#' @export
auto_select_best_kpclr_model = function(explore_kpclr_obj, fp_max_cost = NULL, fn_min_cost = NULL, fp_min_cost = NULL, fn_max_cost = NULL){
	#pull information our for convenience
	fp_cost = explore_kpclr_obj$fp_cost
	fn_cost = explore_kpclr_obj$fn_cost
	num_kernels = explore_kpclr_obj$num_kernels
	num_rhos = explore_kpclr_obj$num_rhos
	fn_over_fp_validation_results = explore_kpclr_obj$fn_over_fp_validation_results
	
	if (is.null(fn_min_cost) && is.null(fn_max_cost) && is.null(fp_min_cost) && is.null(fp_max_cost)){
		min_fn_fp_ratio = (fp_cost / fn_cost) * (1 - 0.25) #This magic number is explained in the documentation
		max_fn_fp_ratio	= (fp_cost / fn_cost) * (1 + 0.25) #This magic number is explained in the documentation
	} else if (!is.null(fn_min_cost) && !is.null(fn_max_cost) && !is.null(fp_min_cost) && !is.null(fp_max_cost)){
		min_fn_fp_ratio = fp_min_cost / fn_max_cost
		max_fn_fp_ratio = fp_max_cost / fn_min_cost
	} else {
		stop("Either fn_min_cost, fn_max_cost, fp_min_cost, fp_max_cost *all* have to be specified or *none* have to specified.")
	}
	
	cost_weighted_errors_validation_with_valid_ratio = matrix(NA, num_kernels, num_rhos)
	for (k in 1 : num_kernels){
		for (j in 1 : num_rhos){
			if (fn_over_fp_validation_results[k, j] >= min_fn_fp_ratio && fn_over_fp_validation_results[k, j] <= max_fn_fp_ratio){
				cost_weighted_errors_validation_with_valid_ratio[k, j] = fn_over_fp_validation_results[k, j]
			}
		}
	}
	if (sum(is.na(cost_weighted_errors_validation_with_valid_ratio)) == num_kernels * num_rhos){
		stop("None of the results were acceptable with the fn/fp ratio bounds provided. Selected model is left unchanged.")
	} else {
		winning = which(cost_weighted_errors_validation_with_valid_ratio == min(cost_weighted_errors_validation_with_valid_ratio, na.rm = TRUE), arr.ind = TRUE)
		explore_kpclr_obj$winning_kernel_num = winning[1]
		explore_kpclr_obj$winning_rho_num = winning[2]
		#also pass back the acceptability bounds for the plot function
		explore_kpclr_obj$min_fn_fp_ratio = min_fn_fp_ratio
		explore_kpclr_obj$max_fn_fp_ratio = max_fn_fp_ratio
	}
	explore_kpclr_obj
}

#' Sets Desired Model
#' 
#' Given an object created with \code{\link{explore_kpcr_models}} or \code{\link{explore_kpclr_models}} and plotted with
#' \code{\link{plot.explore_kpcr}} or \code{\link{plot.explore_kpclr}} respectively, the user now chooses a desired model
#' by selecting a kernel and a rho index from this visualization. This function simply inputs this information into the 
#' object returned by the \code{\link{explore_kpclr_models}} or \code{\link{explore_kpclr_models}} functions. If the user 
#' wishes to reset the desired model, run this function with \code{NULL} for the arguments \code{winning_kernel_num} and
#' \code{winning_rho_num}.
#'  
#' @param explore_kpcr_or_kpclr_obj 	The object created by running \code{\link{explore_kpcr_models}} or \code{\link{explore_kpclr_models}}
#' @param winning_kernel_num 			The desired model's kernel index. 
#' @param winning_rho_num 				The desired model's rho index.
#' @return 								An object of type \code{exlore_kpcr} or \code{explore_kpclr} augmented with 
#' 										information about the user's desired model.
#' 
#' @author 								Adam Kapelner and Justin Bleich
#' 
#' @examples
#' \dontrun{
#' #Note this is example is for classification, but it works the same for regression
#' #first create classification data
#' X = matrix(rnorm(300), ncol = 4)
#' y = rbinom(300, 1, 0.5)
#' #now explore kernel models using the default kernel list
#' explore_kpclr_obj = explore_kpclr_models(X, y)
#' #now we plot to see how the models built on the training data performed on the validation data
#' plot(explore_kpclr_obj)
#' #we believe that the third kernel and the 9th value of rho is the "best" model
#' explore_kpclr_obj = set_desired_model(explore_kpclr_obj, 
#' 						winning_kernel_num = 3, winning_rho_num = 9)
#' #re-plotting shows a blue line indicating the model we just set
#' plot(explore_kpclr_obj)
#' }
#' @export
set_desired_model = function(explore_kpcr_or_kpclr_obj, winning_kernel_num, winning_rho_num){
	checkObjectType(explore_kpcr_or_kpclr_obj, "explore_kpcr_or_kpclr_obj", c("explore_kpcr", "explore_kpclr"), c("explore_kpcr_models", "explore_kpclr_models"))
	explore_kpcr_or_kpclr_obj$winning_kernel_num = winning_kernel_num
	explore_kpcr_or_kpclr_obj$winning_rho_num = winning_rho_num
	#do some resets just for KPCLR objects
	explore_kpcr_or_kpclr_obj$min_fn_fp_ratio = NULL
	explore_kpcr_or_kpclr_obj$max_fn_fp_ratio = NULL
	explore_kpcr_or_kpclr_obj
}
