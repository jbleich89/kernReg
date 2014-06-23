#' Best KPCLR with 3 Splits
#' 
#' Performs the full procedure outlined in the paper. We first take three splits of the data
#' randomly and a cost ratio. Then, we take in a list of kernels. For each kernel, we build a
#' KPCR model on the training data (the first split). Then we look at performance of each kernel
#' over a range of # of principle components (by proportion of kernel matrix explained, \eqn{\rho}). 
#' We pick the proportion that is closest to the predicted cost ratio for each kernel. Now, for 
#' each kernel, we have a model. We then predict all models on the validation data (the second split) 
#' and pick the best model. Using this best model, we predict on the test data (the third split)
#' and report the out-of-sample statistics 
#' 
#' @param X			 		The data's predictor matrix
#' @param y 				The data's response vector	
#' @param kernel_list		A list of kernels to assess performance of over a variety of PC's by \% explained. 
#' 							Each element of this list is a list itself with keys "kernel_type" and "params." 
#' @param seed				A seed to set the random generator. This is required because re-runs will result in
#' 							different training-validation-test splits which would allow a user to snoop on the 
#' 							test data. We default this to 0 but please use your own seeds to avoid seed conflict.
#' @param split_props		When splitting the data into training, validation and test sets respectively, what 
#' 							proportions are assigned to each set? This must be numeric of size 3 and the numbers
#' 							will be normalized to sum to one. The default split is uniform (1/3, 1/3, 1/3).  
#' @param rho_seq			A collection of proportions of the variance explained of the kernel matrix to use
#' @param fn_cost			The costof a false negative (defaults to 1)
#' @param fp_cost			The costof a false positive (defaults to 1)
#' @param plot				Plot the fp/fn as a function of rho for each kernel. Default is \code{TRUE}.
#' @param plot_tile_cols	In the plot, how many performance plots displayed per row. Default is \code{3}.
#' @param eval_test_data	After the "best" model is found using the training and validation data, should we evaluate
#' 							this "best" model on the test data to get a glimpse into out-of-sample performance? Once
#' 							this is done, you cannot "go back" and "try" to assess performance on new kernels as this
#' 							would then be snooping. Thus, this flag is defaulted to \code{FALSE} and should only be
#' 							turned to \code{TRUE} when the user is ready to close the books on this data set and never
#' 							look back.
#' @return 					An object of class \code{best_kplcr} which is a list housing the results of the procedure
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpclr}}
#' @references 				Berk, R., Bleich, J., Kapelner, A.,  Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
best_kpclr_three_splits = function(X, y, 
							kernel_list, 
							seed = 0, 
							split_props = c(1/3, 1/3, 1/3), 
							rho_seq = seq(0.05, 0.95, 0.05), 
							fn_cost = 1,
							fp_cost = 1,
							plot = TRUE, 
							plot_tile_cols = 3, 
							eval_test_data = FALSE){
	set.seed(seed)
	#do a bunch of error checking... lots of things need to be right 
	#in order to divert the function blowing up without the user knowing
	#how to fix it
	checkObjectType(X, "X", "matrix")
	checkObjectType(y, "y", "numeric")
	checkObjectType(kernel_list, "kernel_list", "list", "list()")
	
	n = nrow(X)
	
	if (length(y) != n){
		stop("The response vector must have the same dimension as the predictor matrix")
	}
	
	if (length(split_props) != 3){
		stop("The \"split_props\" parameter must be of length 3.")
	}
	
	#renormalize the proportions for each of the three splits in case the user didn't bother
	split_props = split_props / sum(split_props)
	#the number of observations in each split is just the proportions times the total
	split_counts = round(split_props * n)
	#if we've went over n, shortchange the test set; if we went under n, reward the training set
	if (sum(split_counts) != n){
		diff = n - sum(split_counts)
		if (diff < 0){
			split_counts[3] = split_counts[3] - diff
		} else {
			split_counts[1] = split_counts[1] + diff
		}		
	}
	
	#we need to make sure we can't get a size of 0 for any of the split sizes
	if (any(split_counts == 0)){
		stop("One or more of the \"split_props\" parameter resulted in 0 observations in one or more of the three splits.")
	}
	
	#we need to ensure the rho_seq min/max doesn't result in any # of PC's <= 0 or >= n in the training set
	if (round(min(rho_seq) * split_counts[1]) <= 0){
		stop("The minimum in \"rho_seq\" yields 0 PC's. Please increase this value.")
	} else if (round(max(rho_seq) * split_counts[1]) >= n){
		stop("The maximum in \"rho_seq\" yields n PC's. Please decrease this value.")	
	}
	
	#now get the three splits randomly
	permuted_observation_indices = sample(1 : n)
	split_start_points = c(1, split_counts[1] + 1, split_counts[1] + split_counts[2] + 1)
	split_end_points = c(split_counts[1], split_counts[1] + split_counts[2], n)
	i_training = permuted_observation_indices[split_start_points[1] : split_end_points[1]]
	i_validation = permuted_observation_indices[split_start_points[2] : split_end_points[2]]
	i_test = permuted_observation_indices[split_start_points[3] : split_end_points[3]]
	X_train = X[i_training, ]
	y_train = y[i_training]
	X_validate = X[i_validation, ]
	y_validate = y[i_validation]
	X_test = X[i_test, ]
	y_test = y[i_test]
	
	#now build the kernels
	num_kernels = length(kernel_list)

	
	#construct all the kernels first so if there's an error, the function doesn't blow up after wasting the user's time
	all_kernels = list()
	cat("building all kernels")	
	for (k in 1 : num_kernels){
		checkObjectType(kernel_list[[k]], paste("Element #", k, " of kernel_list", sep = ""), "list", "list()")
		#build a kpca object. The build function will check for all errors.
		all_kernels[[k]] = build_kpca_object(X_train, kernel_list[[k]]$kernel_type, kernel_list[[k]]$params)
		cat(".")
	}
	cat("done\n")
	
	#set up weights
	weights = weights_for_kpclr(y_train, fn_cost / fp_cost)
	
	cat("running all models...\n")
	fn_over_fp_validation_results = matrix(NA, nrow = num_kernels, ncol = length(rho_seq))
	rownames(fn_over_fp_validation_results) = paste("kernel", 1 : num_kernels)
	colnames(fn_over_fp_validation_results) = rho_seq
	cost_weighted_errors_validation = matrix(NA, nrow = num_kernels, ncol = length(rho_seq))
	rownames(cost_weighted_errors_validation) = paste("kernel", 1 : num_kernels)
	colnames(cost_weighted_errors_validation) = rho_seq
	for (k in 1 : num_kernels){
		kpca = all_kernels[[k]]
		desc = kernel_description(kpca)
		cat(desc)
		for (r in 1 : length(rho_seq)){
			rho = rho_seq[r]
			#build model on training data
			mod = kpclr(kpca, y_train, frac_var = rho, weights = weights)
			#predict the model on validation data
			y_validate_hat = predict(mod, X_validate)
			confusion = table(y_validate, ifelse(y_validate_hat > 0.5, 1, 0)) ###FIX LATER!!!
			#for logistic regression, the 0 is a negative and the 1 is a positive
			#thus, since the actual value constitutes the row in the confusion table
			#and the predicted value constitutes the column in the confusion table,
			#the number of "false positives" (actual = 0 and predicted = 1) is located
			#in the (1,2) location in the 2x2 confusion table and the number of "false
			#negatives" (i.e. actual = 1, predicted = 0) is located in the (2,1) location
			#in the 2x2 confusion table
			fn_over_fp_validation_results[k, r] = confusion[2, 1] / confusion[1, 2]
			cost_weighted_errors_validation[k, r] = confusion[2, 1] * fn_cost + confusion[1, 2] * fp_cost
			cat(".")
		}
		cat("\n")
	}
	
	#determine winner
	winning = which(cost_weighted_errors_validation == min(cost_weighted_errors_validation), arr.ind = TRUE)
	winning_kernel_num = winning[1]
	winning_rho_num = winning[2]

	
	#now we're going to plot all fp/fn lines and mark the winning model with a vertical line
	if (plot){
		#set the plotting based on the number of kernels the user wishes to try
		tile_rows = ceiling(num_kernels / plot_tile_cols)	
		par(mfrow = c(tile_rows, plot_tile_cols))
		
		#standardize all 
		fn_over_fp_max = max(fn_over_fp_validation_results)
		cost_max = max(cost_weighted_errors_validation)
		ylim = c(min(fn_over_fp_validation_results), fn_over_fp_max)
		cost_weighted_errors_validation_scaled = cost_weighted_errors_validation / max(cost_weighted_errors_validation) * fn_over_fp_max
		for (k in 1 : num_kernels){
			kpca = all_kernels[[k]]
			desc = kernel_description(kpca)
			
			main = ifelse(k <= plot_tile_cols, paste("validation performance\n", desc), desc)
			
			plot(rho_seq, 
				fn_over_fp_validation_results[k, ], 
				xlab = "rho", 
				ylab = "fp / fn", 
				ylim = ylim,
				main = main, 
				type = "l")
			points(rho_seq, cost_weighted_errors_validation_scaled[k, ], col = "red", type = "l")
			abline(h = fn_over_fp_ratio, col = "gray")
			axis(4, at = fn_over_fp_max, labels = round(cost_max), col = "red")
			if (k == winning_kernel_num){
				abline(v = rho_seq[winning_rho_num], col = "blue", lwd = 3)
			}
		}
	}
	
	#now handle test data
	if (eval_test_data){
		kpca = all_kernels[[winning_kernel_num]]
		mod = kpclr(kpca, y_test, frac_var = rho_seq[winning_rho_num], weights = weights)
		#predict the model on validation data
		y_test_hat = predict(mod, X_test)
		test_confusion = table(y_test, ifelse(y_test_hat > 0.5, 1, 0)) ###FIX LATER!!!
	}
	
	#return everything
	obj = list(
			kernel_list = kernel_list, 
			seed = seed, 
			split_props = split_props, 
			rho_seq = rho_seq, 
			fn_cost = fn_cost,
			fp_cost = fp_cost,
			fn_over_fp_validation_results = fn_over_fp_validation_results,
			cost_weighted_errors_validation = cost_weighted_errors_validation,
			winning_kernel = all_kernels[[winning_kernel_num]], 
			winning_kernel_num = winning_kernel_num,
			winning_rho = rho_seq[winning_rho_num]
	)
	if (eval_test_data){
		obj[["test_confusion"]] = test_confusion
		obj[["test_confusion_proportions"]] = test_confusion / length(y_test)
		obj[["test_misclassification_error"]] = (test_confusion[1, 2] + test_confusion[2, 1]) / length(y_test)
		obj[["test_weighted_cost"]] = test_confusion[2, 1] * fn_cost + test_confusion[1, 2] * fp_cost
	}
	class(obj) = "best_kplcr"
	obj
}

#common code for both these fuctions
best_kpcr_three_splits_common = function(X, y, kernel_list, logistic = FALSE, split_props = c(1/3, 1/3, 1/3), rho_seq = seq(0.05, 0.95, 0.05), fn_to_fp_ratio = 1, plot = TRUE, tile_cols = 3){
	
}