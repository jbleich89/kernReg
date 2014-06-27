#' Explore KPCLR for many kernels
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
#' @param X			 		The data's predictor matrix.
#' @param y 				The data's response vector.
#' @param kernel_list		A list of kernels to assess performance of over a variety of PC's by \% explained. 
#' 							Each element of this list is a list itself with keys "kernel_type" and "params." If
#' 							left unspecified, the default are four ANOVA models: (1) sigma = 0.1, d = 2 (2) sigma = 100
#' 							d = 2 (3) sigma = 0.1, d = 3 (4) sigma = 100, d = 3
#' @param seed				A seed to set the random generator. This is required because re-runs will result in
#' 							different training-validation-test splits which would allow a user to snoop on the 
#' 							test data. We default this to 0 but please use your own seeds to avoid seed conflict.
#' @param split_props		When splitting the data into training, validation and test sets respectively, what 
#' 							proportions are assigned to each set? This must be numeric of size 3 and the numbers
#' 							will be normalized to sum to one. The default split is uniform (1/3, 1/3, 1/3).  
#' @param rho_seq			A collection of proportions of the variance explained of the kernel matrix to use. The default
#' 							is 30\%, 35\%, ..., 95\%.
#' @param fn_cost			The target cost of a false negative (defaults to 1). Together with \code{fp_cost},
#' 							this will inform the algorithm of the desired ratio of costs.
#' @param fp_cost			The target cost of a false positive (defaults to 1). Together with \code{fn_cost},
#' 							this will inform the algorithm of the desired cost ratio of fp:fn.
#' @param family			The family parameter to be passed to the glm function. Default is "binomial." Note that when family 
#' 							is set to "quasibinomial," AIC calculations are not possible.
#' @param num_cores			The number of cores to use in parallel during computation.
#' 
#' @return 					An object of class \code{explore_kplcr} which is a list housing the results of the procedure
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpclr}}
#' @references 				Berk, R., Bleich, J., Kapelner, A., Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
explore_kpclr_models = function(X, y, 
								kernel_list = NULL, 
								seed = 0, 
								split_props = c(1/3, 1/3, 1/3), 
								rho_seq = seq(from = 0.30, to = 0.95, by = 0.05),
								fn_cost = 1,
								fp_cost = 1,
								family = "binomial",
								num_cores = 1){

	obj = explore_common(X, y, kernel_list, seed, split_props, rho_seq)
	if (length(names(table(y))) != 2){
		stop("The response variable must be zeroes and ones and must have examples of each.")
	}
	if (names(table(y)) != c(0, 1)){
		stop("The response variable must be zeroes and ones and must have examples of each.")
	}
	
	rho_seq = obj$rho_seq
	num_rhos = obj$num_rhos
	split_props = obj$split_props
	num_kernels = obj$num_kernels
	all_kernels = obj$all_kernels
	
	#set up weights
	weights = weights_for_kpclr(obj$y_train, fn_cost / fp_cost)
	
	cat("running all models...\n")
	fn_over_fp_validation_results = matrix(NA, nrow = num_kernels, ncol = num_rhos)
	rownames(fn_over_fp_validation_results) = paste("kernel", 1 : num_kernels)
	colnames(fn_over_fp_validation_results) = rho_seq
	cost_weighted_errors_validation = matrix(NA, nrow = num_kernels, ncol = num_rhos)
	rownames(cost_weighted_errors_validation) = paste("kernel", 1 : num_kernels)
	colnames(cost_weighted_errors_validation) = rho_seq
	mod_aics = matrix(NA, nrow = num_kernels, ncol = num_rhos)
	rownames(mod_aics) = paste("kernel", 1 : num_kernels)
	colnames(mod_aics) = rho_seq
	
	for (k in 1 : num_kernels){
		kpca = all_kernels[[k]]
		desc = kernel_description(kpca)
		cat(desc)
		for (r in 1 : num_rhos){
			rho = rho_seq[r]
			#build model on training data
			mod = kpclr(kpca, obj$y_train, frac_var = rho, weights = weights, family = family)
			mod_aics[k, r] = AIC(mod)
			#predict the model on validation data
			y_validate_hat = predict(mod, obj$X_validate, num_cores = num_cores)
			confusion = table(c(obj$y_validate, 0, 1), c(ifelse(y_validate_hat > 0.5, 1, 0), 0, 1)) ###FIX LATER!!!
			confusion = confusion - diag(2)
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
#			print(rho)
#			print(confusion)
		}
		cat("\n")
	}
	
	#return everything that the plot function will need
	obj$family = family
	obj$fn_cost = fn_cost
	obj$fp_cost = fp_cost
	obj$fn_over_fp_validation_results = fn_over_fp_validation_results
	obj$cost_weighted_errors_validation = cost_weighted_errors_validation
	obj$mod_aics = mod_aics
	class(obj) = "explore_kpclr"
	obj
}

#' Explore KPCR for many kernels
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
#' 							Each element of this list is a list itself with keys "kernel_type" and "params." If
#' 							left unspecified, the default are four ANOVA models: (1) sigma = 0.1, d = 2 (2) sigma = 100
#' 							d = 2 (3) sigma = 0.1, d = 3 (4) sigma = 100, d = 3
#' @param seed				A seed to set the random generator. This is required because re-runs will result in
#' 							different training-validation-test splits which would allow a user to snoop on the 
#' 							test data. We default this to 0 but please use your own seeds to avoid seed conflict.
#' @param split_props		When splitting the data into training, validation and test sets respectively, what 
#' 							proportions are assigned to each set? This must be numeric of size 3 and the numbers
#' 							will be normalized to sum to one. The default split is uniform (1/3, 1/3, 1/3).  
#' @param rho_seq			A collection of proportions of the variance explained of the kernel matrix to use. The default
#' 							is 30\%, 35\%, ..., 95\%.
#' @param num_cores			The number of cores to use in parallel during computation.
#' 
#' 
#' @return 					An object of class \code{explore_kplcr} which is a list housing the results of the procedure.
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpcr}}
#' @references 				Berk, R., Bleich, J., Kapelner, A., Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
explore_kpcr_models = function(X, y, 
		kernel_list = NULL, 
		seed = 0, 
		split_props = c(1/3, 1/3, 1/3), 
		rho_seq = seq(from = 0.30, to = 0.95, by = 0.05),
		num_cores = 1){
	
	obj = explore_common(X, y, kernel_list, seed, split_props, rho_seq)
	
	#make sure the user isn't by accident doing classification in which case they should use the other function
	if (length(table(y)) == 2){
		warning("Only two levels detected in response. Are you sure you are not doing classification?\nIf so, please use the \"explore_kpclr_models\" function.")	
	}
	
	rho_seq = obj$rho_seq
	num_rhos = obj$num_rhos
	split_props = obj$split_props
	num_kernels = obj$num_kernels
	all_kernels = obj$all_kernels
	
	cat("running all models...\n")
	sse_validation_results = matrix(NA, nrow = num_kernels, ncol = num_rhos)
	rownames(sse_validation_results) = paste("kernel", 1 : num_kernels)
	colnames(sse_validation_results) = rho_seq
	mod_aics = matrix(NA, nrow = num_kernels, ncol = num_rhos)
	rownames(mod_aics) = paste("kernel", 1 : num_kernels)
	colnames(mod_aics) = rho_seq
	
	for (k in 1 : num_kernels){
		kpca = all_kernels[[k]]
		desc = kernel_description(kpca)
		cat(desc)
		for (r in 1 : num_rhos){
			rho = rho_seq[r]
			#build model on training data
			mod = kpcr(kpca, obj$y_train, frac_var = rho)
			mod_aics[k, r] = AIC(mod)
			#predict the model on validation data
			y_validate_hat = predict(mod, obj$X_validate, num_cores = num_cores)
			#compute and store the out-of-sample SSE
			sse_validation_results[k, r] = sum((obj$y_validate - y_validate_hat)^2)
			cat(".")
#			print(rho)
#			print(sse_validation_results[k, r])
		}
		cat("\n")		
	}
	
	#return everything that the plot function will need
	obj$sse_validation_results = sse_validation_results
	obj$mod_aics = mod_aics
	class(obj) = "explore_kpcr"
	obj
}

#common code for both fuctions
explore_common = function(X, y, kernel_list, seed, split_props, rho_seq){
	set.seed(seed)
	#do a bunch of error checking... lots of things need to be right 
	#in order to divert the function blowing up without the user knowing
	#how to fix it
	checkObjectType(X, "X", "matrix")
	checkObjectType(y, "y", "numeric")	
	
	#create a default kernel if the user did not specify one
	if (is.null(kernel_list)){
		kernel_list = list()
		kernel_list[[1]] = list(kernel_type = "anova", params = c(0.1, 2))
		kernel_list[[2]] = list(kernel_type = "anova", params = c(100, 2))
		kernel_list[[3]] = list(kernel_type = "anova", params = c(0.1, 3))
		kernel_list[[4]] = list(kernel_type = "anova", params = c(100, 3))
	}
	
	checkObjectType(kernel_list, "kernel_list", "list", "list()")
	
	n = nrow(X)
	
	#check that y is sized the same as X
	if (length(y) != n){
		stop("The response vector must have the same dimension as the predictor matrix")
	}

	
	#check that split_props is correctly formatted
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
	
	obj = list(
		kernel_list = kernel_list,
		n = n,
		seed = seed,
		split_props = split_props, 
		rho_seq = rho_seq,
		num_rhos = length(rho_seq),
		X_train = X_train,
		y_train = y_train,
		X_validate = X_validate,
		y_validate = y_validate,
		X_test = X_test,
		y_test = y_test,
		n_train = length(y_train),
		n_validate = length(y_validate),
		n_test = length(y_test),
		num_kernels = num_kernels,
		all_kernels = all_kernels
	)
	obj
}