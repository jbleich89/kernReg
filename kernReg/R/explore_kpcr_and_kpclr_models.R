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
#' 							Each element of this list is a list itself with keys "kernel_type" and "params." 
#' @param seed				A seed to set the random generator. This is required because re-runs will result in
#' 							different training-validation-test splits which would allow a user to snoop on the 
#' 							test data. We default this to 0 but please use your own seeds to avoid seed conflict.
#' @param split_props		When splitting the data into training, validation and test sets respectively, what 
#' 							proportions are assigned to each set? This must be numeric of size 3 and the numbers
#' 							will be normalized to sum to one. The default split is uniform (1/3, 1/3, 1/3).  
#' @param rho_seq			A collection of proportions of the variance explained of the kernel matrix to use.
#' @param fn_cost			The target cost of a false negative (defaults to 1). Together with \code{fp_cost},
#' 							this will inform the algorithm of the desired ratio of costs.
#' @param fp_cost			The target cost of a false positive (defaults to 1). Together with \code{fn_cost},
#' 							this will inform the algorithm of the desired cost ratio of fp:fn.
#' @param fp_max_cost		The maximum cost of a false positve. Together with \code{fn_min_cost}, this will inform
#' 							the algorithm of the maximum cost ratio of fp:fn. If left to the default \code{NULL}, the maximum
#' 							cost ratio will be 25\% more than the desired cost ratio.
#' @param fn_min_cost		The minimum cost of a false negative. Together with \code{fp_max_cost}, this will inform
#' 							the algorithm of the maximum cost ratio of fp:fn. If left to the default \code{NULL}, the maximum
#' 							cost ratio will be 25\% more than the desired cost ratio. 
#' @param fp_min_cost		The minimum cost of a false positive. Together with \code{fn_max_cost}, this will inform
#' 							the algorithm of the minimum cost ratio of fp:fn. If left to the default \code{NULL}, the minimum
#' 							cost ratio will be 25\% less than the desired cost ratio. 
#' @param fn_max_cost		The maximum cost of a false negative. Together with \code{fp_min_cost}, this will inform
#' 							the algorithm of the minimum cost ratio of fp:fn. If left to the default \code{NULL}, the minimum
#' 							cost ratio will be 25\% less than the desired cost ratio. 
#' 
#' 
#' @return 					An object of class \code{explore_kplcr} which is a list housing the results of the procedure
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpclr}}
#' @references 				Berk, R., Bleich, J., Kapelner, A., Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
explore_kpclr_models = function(X, y, 
								kernel_list, 
								seed = 0, 
								split_props = c(1/3, 1/3, 1/3), 
								rho_seq = seq(0.05, 0.95, 0.05), 
								fn_cost = 1,
								fp_cost = 1,
								fp_max_cost = NULL,
								fn_min_cost = NULL,
								fp_min_cost = NULL,
								fn_max_cost = NULL,								
								num_cores = 1){
							
	if (is.null(fn_min_cost) && is.null(fn_max_cost) && is.null(fp_min_cost) && is.null(fp_max_cost)){
		min_fn_fp_ratio = (fp_cost / fn_cost) * 0.75
		max_fn_fp_ratio	= (fp_cost / fn_cost) * 1.25
	} else if (!is.null(fn_min_cost) && !is.null(fn_max_cost) && !is.null(fp_min_cost) && !is.null(fp_max_cost)){
		min_fn_fp_ratio = fp_min_cost / fn_max_cost
		max_fn_fp_ratio = fp_max_cost / fn_min_cost
	} else {
		stop("Either fn_min_cost, fn_max_cost, fp_min_cost, fp_max_cost all have to be specified or none have to specified.")
	}							

	obj = explore_common(X, y, kernel_list, seed, split_props, rho_seq)
	rho_seq = obj$rho_seq
	num_rhos = obj$num_rhos
	split_props = obj$split_props
	num_kernels = obj$num_kernels
	all_kernels = obj$all_kernels
	y_train = obj$y_train
	
	#set up weights
	weights = weights_for_kpclr(y_train, fn_cost / fp_cost)
	
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
	
	#will work on windows -- not sure about unix/mac
#	cluster = makeCluster(num_cores)
#	registerDoParallel(cluster)	
	
#	kernel_results = foreach(k = 1 : num_kernels) %dopar% {
#		library(kernlab)
#		library(kernReg)
#		
#		iter_list = list()
	for (k in 1 : num_kernels){
		kpca = all_kernels[[k]]
		desc = kernel_description(kpca)
		cat(desc)
		for (r in 1 : num_rhos){
			rho = rho_seq[r]
			#build model on training data
			mod = kpclr(kpca, y_train, frac_var = rho, weights = weights)
			mod_aics[k, r] = AIC(mod)
			#predict the model on validation data
			y_validate_hat = predict(mod, obj$X_validate)
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
	
	#determine winner
	cost_weighted_errors_validation_with_valid_ratio = ifelse(fn_over_fp_validation_results >= min_fn_fp_ratio && fn_over_fp_validation_results <= max_fn_fp_ratio, fn_over_fp_validation_results, NA)
	if (sum(is.na(cost_weighted_errors_validation_with_valid_ratio)) == num_kernels * num_rhos){
		cat("WARNING: None of the results were acceptable with the fn/fp ratio bounds provided.\n")
		winning_kernel_num = NA
		winning_rho_num = NA
	} else {
		winning = which(cost_weighted_errors_validation_with_valid_ratio == min(cost_weighted_errors_validation_with_valid_ratio, na.rm = TRUE), arr.ind = TRUE)
		winning_kernel_num = winning[1]
		winning_rho_num = winning[2]
	}

	#return everything
	obj$fn_cost = fn_cost
	obj$fp_cost = fp_cost
	obj$min_fn_fp_ratio = min_fn_fp_ratio
	obj$max_fn_fp_ratio = max_fn_fp_ratio
	obj$fn_over_fp_validation_results = fn_over_fp_validation_results
	obj$cost_weighted_errors_validation = cost_weighted_errors_validation
	obj$winning_kernel = all_kernels[[winning_kernel_num]]
	obj$winning_kernel_num = winning_kernel_num
	obj$winning_rho_num = winning_rho_num
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
#' 							Each element of this list is a list itself with keys "kernel_type" and "params." 
#' @param seed				A seed to set the random generator. This is required because re-runs will result in
#' 							different training-validation-test splits which would allow a user to snoop on the 
#' 							test data. We default this to 0 but please use your own seeds to avoid seed conflict.
#' @param split_props		When splitting the data into training, validation and test sets respectively, what 
#' 							proportions are assigned to each set? This must be numeric of size 3 and the numbers
#' 							will be normalized to sum to one. The default split is uniform (1/3, 1/3, 1/3).  
#' @param rho_seq			A collection of proportions of the variance explained of the kernel matrix to use.
#' @param num_cores			The number of cores to use in parallel during computation.
#' 
#' 
#' @return 					An object of class \code{explore_kplcr} which is a list housing the results of the procedure.
#' 
#' @author 					Justin Bleich and Adam Kapelner
#' @seealso 				\code{\link{kpclr}}
#' @references 				Berk, R., Bleich, J., Kapelner, A., Henderson, J. and Kurtz, E., Using Regression Kernels to Forecast A Failure to Appear in Court. (2014) working paper
#' @export
explore_kpcr_models = function(X, y, 
		kernel_list, 
		seed = 0, 
		split_props = c(1/3, 1/3, 1/3), 
		rho_seq = seq(0.05, 0.95, 0.05),
		num_cores = 1){
	
	obj = explore_common(X, y, kernel_list, seed, split_props, rho_seq)
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
	
	#will work on windows -- not sure about unix/mac
#	cluster = makeCluster(num_cores)
#	registerDoParallel(cluster)	
	
#	kernel_results = foreach(k = 1 : num_kernels) %dopar% {
#		library(kernlab)
#		library(kernReg)
#		
#		iter_list = list()
	for (k in 1 : num_kernels){
		kpca = all_kernels[[k]]
		desc = kernel_description(kpca)
		cat(desc)
		for (r in 1 : num_rhos){
			rho = rho_seq[r]
			#build model on training data
			mod = kpcr(kpca, y_train, frac_var = rho)
			mod_aics[k, r] = AIC(mod)
			#predict the model on validation data
			y_validate_hat = predict(mod, X_validate)
			#compute and store the out-of-sample SSE
			sse_validation_results[k, r] = sum((y_validate - y_validate_hat)^2)
			cat(".")
#			print(rho)
#			print(sse_validation_results[k, r])
		}
		cat("\n")		
	}
	
	#determine winner
	winning = which(sse_validation_results == min(sse_validation_results), arr.ind = TRUE)
	winning_kernel_num = winning[1]
	winning_rho_num = winning[2]
	
	#return everything
	obj$sse_validation_results = sse_validation_results
	obj$winning_kernel = all_kernels[[winning_kernel_num]]
	obj$winning_kernel_num = winning_kernel_num
	obj$winning_rho_num = winning_rho_num
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
	
	obj = list(
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

#' Evaluate Test Data
#'
#' After a "satisfactory" model is selected by the user using the \code{explore_kpcr_models} function,
#' we now predict on the test data to get a glimpse into this model's future out-of-sample performance. 
#' Warning: once this is done, you cannot "go back" and "try" to assess performance on new kernels as this
#' would then be snooping. Run this function when you are ready to close the books on this data set and never
#' look back.
#' 
#' @param explore_kpclr 			This object is built from \code{explore_kplcr_models}. We assume the user has updated 
#' 									this object with a satisfactory model by settings \code{winning_kernel_num} to denote 
#' 									which kernel is selected for the final model and setting \code{winning_rho_num} to 
#' 									denote which proportion of the variance of the kernel matrix is selected for the 
#' 									final model.
#' 
#' @return 							An expanded \code{explore_kpclr} list object with new entries
#' 									that contain information about the performance of the final model on the
#' 									test data: \code{test_confusion}, the confusion matrix of the test data; 
#' 									\code{test_confusion_proportions}, the confusion matrix of the test data
#' 									as proportions of the number of test observations; \code{test_misclassification_error}, 
#' 									the total misclassification error of the test data and \code{test_weighted_cost},  
#' 									the cost of the errors made as defined by the \code{fn_cost} and \code{fp_cost}
#' 									specified by the user when constructing the model via \code{explore_kplcr_models}.
#' 
#' @seealso 						code{explore_kplcr_models}
#' @author 							Adam Kapelner and Justin Bleich
#' @export
eval_winning_lr_model_on_test_data = function(winning_model, explore_kpclr){
	#predict the model on test data and build a confusion matrix
	#predict the model on training and validation data
	winning_kernel_info = explore_kpclr$kernel_list[[explore_kpclr$winning_kernel_num]]
	X_train_and_validate = rbind(explore_kpclr$X_train, explore_kpclr$X_validate)
	kpca = build_kpca_object(X_train_and_validate, winning_kernel_info$kernel_type, winning_kernel_info$params)
	y_train_and_validate = c(explore_kpclr$y_train, explore_kpclr$y_validate)
	weights = weights_for_kpclr(y_train_and_validate, explore_kpclr$fn_cost / explore_kpclr$fp_cost)
	winning_model = kpclr(kpca, y_train_and_validate, frac_var = explore_kpclr$rho_seq[explore_kpclr$winning_rho_num], weights = weights)	
	y_test_hat = predict(winning_model, explore_kpclr$X_test)
	test_confusion = table(explore_kpclr$y_test, ifelse(y_test_hat > 0.5, 1, 0)) ###FIX LATER!!!
	#pass back the data
	explore_kpclr[["test_confusion"]] = test_confusion
	explore_kpclr[["test_confusion_proportions"]] = test_confusion / explore_kpclr$n_test
	explore_kpclr[["test_misclassification_error"]] = (test_confusion[1, 2] + test_confusion[2, 1]) / explore_kpclr$n_test
	explore_kpclr[["test_weighted_cost"]] = test_confusion[2, 1] * fn_cost + test_confusion[1, 2] * fp_cost
	explore_kpclr
}

#' Evaluate Test Data
#'
#' After a "satisfactory" model is selected by the user using the \code{explore_kpcr_models} function,
#' we now predict on the test data to get a glimpse into this model's future out-of-sample performance. 
#' Warning: once this is done, you cannot "go back" and "try" to assess performance on new kernels as this
#' would then be snooping. Run this function when you are ready to close the books on this data set and never
#' look back.
#' 
#' @param explore_kpcr 				This object is built from \code{explore_kpcr_models}. We assume the user has updated 
#' 									this object with a satisfactory model by settings \code{winning_kernel_num} to denote 
#' 									which kernel is selected for the final model and setting \code{winning_rho_num} to 
#' 									denote which proportion of the variance of the kernel matrix is selected for the 
#' 									final model.
#'
#' @return 							An expanded \code{explore_kpcr} list object with new entries
#' 									that contain information about the performance of the final model on the
#' 									test data: \code{L2_err}, the sum of squared error; \code{rmse}, the root 
#' 									mean squared error and \code{L1_err}, the sum of absolute error.
#' 
#' @seealso 						\code{\link{explore_kplr_models}}
#' @author 							Adam Kapelner and Justin Bleich
#' @export
eval_winning_r_model_on_test_data = function(explore_kpcr){
	#predict the model on training and validation data
	winning_kernel_info = explore_kpcr$kernel_list[[explore_kpcr$winning_kernel_num]]
	X_train_and_validate = rbind(explore_kpcr$X_train, explore_kpcr$X_validate)
	kpca = build_kpca_object(X_train_and_validate, winning_kernel_info$kernel_type, winning_kernel_info$params)
	y_train_and_validate = c(explore_kpcr$y_train, explore_kpcr$y_validate)
	winning_model = kpcr(kpca, y_train_and_validate, frac_var = explore_kpcr$rho_seq[explore_kpcr$winning_rho_num])
	#build the model and return	
		
	
	weights = weights_for_kpclr(y_train_and_validate, explore_kpcr$fn_cost / explore_kpcr$fp_cost)
	kpclr(kpca, y_train_and_validate, frac_var = explore_kpcr$rho_seq[explore_kpcr$winning_rho_num], weights = weights)	

	y_test_hat = predict(winning_model, explore_kpcr$X_test)
	#pass back the performance data
	sse = sum((y_test_hat - exploration_kpclr$y_test)^2)
	exploration_kpclr[["L2_err"]] = sse
	exploration_kpclr[["rmse"]] = sqrt(sse / explore_kpcr$n_test)	
	exploration_kpclr[["L1_err"]] = sum(abs(y_test_hat - explore_kpcr$y_test))
	exploration_kpclr
}


#' Create Final Kernel Model
#' 
#' Once the user has finished exploring different kernel regressions via \code{\link{explore_kplcr_models}} or
#' \code{\link{explore_kplr_models}} and has estimated future performance on the test data via 
#' \code{\link{eval_winning_r_model_on_test_data}} or \code{\link{eval_winning_lr_model_on_test_data}},
#' we now build the final kernel model using all the data from \code{X}, \code{y}.
#' 
#' @param explore_kpclr_or_kpcr 	The object built from \code{explore_kplr_models} or \code{explore_kplcr_models}.
#' @return 							The model corresponding to the \code{winning_kernel_num} and the 
#' 									\code{winning_rho_num} housed in the explore object.
#' 
#' @author 							Adam Kapelner and Justin Bleich
#' @export
build_final_kpclr_or_kpcr_model = function(explore_kpclr_or_kpcr){
	#pull out the winning kernel and rebuild this kernel on both the training and validation data
	winning_kernel_info = explore_kpclr_or_kpcr$kernel_list[[explore_kpclr_or_kpcr$winning_kernel_num]]
	X = rbind(explore_kpclr_or_kpcr$X_train, explore_kpclr_or_kpcr$X_validate, explore_kpclr_or_kpcr$X_test)
	kpca = build_kpca_object(X, winning_kernel_info$kernel_type, winning_kernel_info$params)
	y = c(explore_kpclr_or_kpcr$y_train, explore_kpclr_or_kpcr$y_validate, explore_kpclr_or_kpcr$y_test)
	#build the model and return
	if (class(explore_kpclr_or_kpcr) == "explore_kpclr"){		
		weights = weights_for_kpclr(y, explore_kpclr_or_kpcr$fn_cost / explore_kpclr_or_kpcr$fp_cost)
		kpclr(kpca, y, frac_var = explore_kpclr_or_kpcr$rho_seq[explore_kpclr_or_kpcr$winning_rho_num], weights = weights)	
	} else if (class(explore_kpclr_or_kpcr) == "explore_kpclr"){
		kpcr(kpca, y, frac_var = explore_kpclr_or_kpcr$rho_seq[explore_kpclr_or_kpcr$winning_rho_num])
	}	
}