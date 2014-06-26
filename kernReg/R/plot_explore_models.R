#' Explore kpclr plot
#' 
#' Many kernel principle components logistic regression models were fitted on the training data via 
#' \code{\link{explore_kpclr_models}}. This function will create one plot for each of the kernel models investigated.
#' At every value of rho (the proportion of variance of the kernel matrix explained), the following three things 
#' will be plotted (1) the ratio of the number of false negatives to the number of false positives when predicted
#' out of sample (2) the AIC of the model (3) the cost-weighted error of the out-of-sample validation data.
#' 
#' @param explore_kpclr_obj 			An object of type \code{explore_kpclr} built with \code{\link{explore_kpclr_models}}
#' @param tile_cols 					When plotting all kernel model performances, how many kernels per plot window column?
#' 										Default is \code{3}.
#' @param min_fn_fp_ratio				If specified, plots a horizontal line on the y-axis representing the lower bound of
#' 										the ratio of the number of false negatives to false positives. Defaults to the value set
#' 										by \code{\link{auto_select_best_kpclr_model}} (if it was run previously). If not, no line
#' 										is plotted.
#' @param max_fn_fp_ratio				If specified, plots a horizontal line on the y-axis representing the upper bound of
#' 										the ratio of the number of false negatives to false positives. Defaults to the value set
#' 										by \code{\link{auto_select_best_kpclr_model}} (if it was run previously). If not, no line
#' 										is plotted.
#' @param quantile_aic_to_display 		When plotting the AICs for each model, which quantile should be truncated?
#' 										Default is \code{95\%}.
#' @param quantile_cwe_to_display 		When plotting the cost-weighted-errors for each model, which quantile should be 
#' 										truncated? Default is \code{75\%}.
#' @param color_winning_model 			What color is the vertical line of the winning model. Default is blue.
#' @param color_num_fn_fp_ratio			What color are the ratios of false negatives to false positive? The default is black.
#' @param color_aic 					What color are the AIC lines? Default is reddish.
#' @param color_cwe 					What color are the cost weighted error lines? Default is greenish.
#' @param show_rho_numbers 				Plot the rho number on each of the exploratory plots. Default is \code{TRUE}.
#' @param text_label_offset_pct			If the rho numbers are plotted, what percent offset below the points? Default is \code{10\%}.
#' @param ... 							Other parameters to pass to plot. Of particular interest is \code{xlim} which will limit
#' 										some models from being displayed.
#' 
#' @author 								Adam Kapelner and Justin Bleich
#' @method plot explore_kpclr
#' @export
plot.explore_kpclr = function(explore_kpclr_obj,
		tile_cols = 3, 
		min_fn_fp_ratio = NULL,
		max_fn_fp_ratio = NULL,
		quantile_aic_to_display = 0.75, 
		quantile_cwe_to_display = 0.95,
		color_winning_model = "blue",
		color_num_fn_fp_ratio = "black",
		color_aic = "firebrick3",
		color_cwe = "forestgreen",
		show_rho_numbers = TRUE,
		text_label_offset_pct = 0.10,
		...){
	#pull out data for convenience
	rho_seq = explore_kpclr_obj$rho_seq
	desired_fn_fp_ratio = explore_kpclr_obj$fp_cost / explore_kpclr_obj$fn_cost
	
	if (is.null(min_fn_fp_ratio)){
		min_fn_fp_ratio = explore_kpclr_obj$min_fn_fp_ratio
	}
	if (is.null(max_fn_fp_ratio)){
		max_fn_fp_ratio = explore_kpclr_obj$max_fn_fp_ratio	
	}
		
	num_kernels = explore_kpclr_obj$num_kernels
	fn_over_fp_validation_results = explore_kpclr_obj$fn_over_fp_validation_results
	cost_weighted_errors_validation = explore_kpclr_obj$cost_weighted_errors_validation
	mod_aics = explore_kpclr_obj$mod_aics
	
	#now we're going to plot all fp/fn lines and mark the winning model with a vertical line
	#set the plotting based on the number of kernels the user wishes to try
	tile_rows = ceiling(explore_kpclr_obj$num_kernels / tile_cols)	
	par(mfrow = c(tile_rows, tile_cols))
	text_label_indices = seq(from = 1, to = length(rho_seq), by = 3)
	
	#standardize all plots to have the same axes
	fn_over_fp_max = 3 * max(desired_fn_fp_ratio, desired_fn_fp_ratio^-1)
	fn_over_fp_min = min(3 * min(desired_fn_fp_ratio, desired_fn_fp_ratio^-1), fn_over_fp_validation_results, min_fn_fp_ratio)
	cost_max = max(cost_weighted_errors_validation)
	ylim = c(fn_over_fp_min, fn_over_fp_max)
	text_label_offset = text_label_offset_pct * (ylim[2] - ylim[1])
	cost_weighted_errors_validation_scaled = cost_weighted_errors_validation / quantile(cost_weighted_errors_validation, quantile_cwe_to_display, na.rm = TRUE) * fn_over_fp_max
	mod_aics_scaled = mod_aics / quantile(mod_aics, quantile_aic_to_display, na.rm = TRUE) * fn_over_fp_max
	
	for (k in 1 : num_kernels){
		kpca = explore_kpclr_obj$all_kernels[[k]]
		main = paste("#", k, " ", kernel_description(kpca), sep = "")
		
		plot(rho_seq, 
				fn_over_fp_validation_results[k, ], 
				xlab = "rho", 
				ylab = "# fn / # fp", 
				ylim = ylim,
				main = main,
				col = color_num_fn_fp_ratio,
				type = "o",
				...)
		
		#graph the desired ratio line
		abline(h = desired_fn_fp_ratio, col = "gray") 
		
		#if the user specified bounds, plot these too
		if (!is.null(min_fn_fp_ratio) && !is.null(max_fn_fp_ratio)){
			abline(h = min_fn_fp_ratio, col = rgb(0.95, 0.95, 0.95))
			abline(h = max_fn_fp_ratio, col = rgb(0.95, 0.95, 0.95))			
		}

		points(rho_seq, cost_weighted_errors_validation_scaled[k, ], col = "red", type = "o")
		points(rho_seq, mod_aics_scaled[k, ], col = "forestgreen", type = "o")
		if (show_rho_numbers){
			text(rho_seq[text_label_indices], fn_over_fp_validation_results[k, text_label_indices] - text_label_offset, text_label_indices)	
		}		
		if (sum(is.na(mod_aics_scaled[k, ])) == length(rho_seq)){
			axis(4, at = fn_over_fp_max, labels = round(quantile(cost_weighted_errors_validation, quantile_cwe_to_display, na.rm = TRUE)))
			
		} else {
			axis(4, at = fn_over_fp_max, labels = paste(round(quantile(cost_weighted_errors_validation, quantile_cwe_to_display, na.rm = TRUE)), "/", round(quantile(mod_aics, quantile_aic_to_display, na.rm = TRUE))))
		}
		
		if (!is.null(explore_kpclr_obj$winning_kernel_num) && !is.na(explore_kpclr_obj$winning_kernel_num) && k == explore_kpclr_obj$winning_kernel_num){
			abline(v = rho_seq[explore_kpclr_obj$winning_rho_num], col = "blue", lwd = 3)
		}
	}
}


#' Explore kpcr plot
#' 
#' Many kernel principle components logistic regression models were fitted on the training data via 
#' \code{\link{explore_kpcr_models}}. This function will create one plot for each of the kernel models investigated.
#' At every value of rho (the proportion of variance of the kernel matrix explained), the AIC of the model and the sse
#' of the out-of-sample validation data is plotted.
#' 
#' @param explore_kpcr_obj 				An object of type \code{explore_kpcr} built with \code{\link{explore_kpcr_models}}
#' @param tile_cols 					When plotting all kernel model performances, how many kernels per plot window column?
#' 										Default is \code{3}.
#' @param quantile_aic_to_display 		When plotting the AICs for each model, which quantile should be truncated?
#' 										Default is \code{75\%}.
#' @param color_winning_model 			What color is the vertical line of the winning model. Default is blue.
#' @param color_aic 					What color are the AICs? Default is reddish.
#' @param color_sse 					What color are the SSEs? Default is greenish.
#' @param show_rho_numbers 				Plot the rho number on each of the exploratory plots. Default is \code{TRUE}.
#' @param text_label_offset_pct			If the rho numbers are plotted, what percent offset below the points? Default is \code{10\%}.
#' @param label_skip					If the rho numbers are plotted, how many should be skipped? Default is \code{3}.
#' @param ... 							Other parameters to pass to plot.
#' 
#' @author 								Adam Kapelner and Justin Bleich
#' @method plot explore_kpcr
#' @export
plot.explore_kpcr = function(explore_kpcr_obj, 
		tile_cols = 3,
		quantile_aic_to_display = 0.75,
		color_winning_model = "blue",
		color_sse = "black",
		color_aic = "firebrick3",		
		show_rho_numbers = TRUE,
		text_label_offset_pct = 0.10,
		label_skip = 3,
		...){
	#pull out data for convenience
	rho_seq = explore_kpcr_obj$rho_seq
	num_kernels = explore_kpcr_obj$num_kernels
	sse_validation_results = explore_kpcr_obj$sse_validation_results
	mod_aics = explore_kpcr_obj$mod_aics
	
	#now we're going to plot all fp/fn lines and mark the winning model with a vertical line
	#set the plotting based on the number of kernels the user wishes to try
	tile_rows = ceiling(explore_kpcr_obj$num_kernels / tile_cols)	
	par(mfrow = c(tile_rows, tile_cols))
	text_label_indices = seq(from = 1, to = length(rho_seq), by = label_skip)
	
	#standardize all plots to have the same axes	
	ylim = c(min(sse_validation_results), max(sse_validation_results))
	text_label_offset = text_label_offset_pct * (ylim[2] - ylim[1])
	mod_aics_scaled = mod_aics / quantile(mod_aics, quantile_aic_to_display, na.rm = TRUE) * ylim[2]
	
	for (k in 1 : num_kernels){
		kpca = explore_kpcr_obj$all_kernels[[k]]
		main = paste("#", k, " ", kernel_description(kpca), sep = "")
		
		plot(rho_seq, 
				sse_validation_results[k, ], 
				xlab = "rho", 
				ylab = "sse", 
				ylim = ylim,
				main = main, 
				type = "o",
				col = color_sse,
				...)
		points(rho_seq, mod_aics_scaled[k, ], col = "forestgreen", type = "o")
		if (show_rho_numbers){
			text(rho_seq[text_label_indices], sse_validation_results[k, text_label_indices] - text_label_offset, text_label_indices)	
		}		
		axis(4, at = ylim[2], labels = paste(round(quantile(mod_aics, quantile_aic_to_display, na.rm = TRUE))))
		if (!is.null(explore_kpcr_obj$winning_kernel_num) && !is.na(explore_kpcr_obj$winning_kernel_num) && k == explore_kpcr_obj$winning_kernel_num){
			abline(v = rho_seq[explore_kpcr_obj$winning_rho_num], col = "blue", lwd = 3)
		}
	}
}

