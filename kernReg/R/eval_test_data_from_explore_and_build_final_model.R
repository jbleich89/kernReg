#' Evaluate Test Data
#'
#' After a "satisfactory" model is selected by the user using the \code{explore_kpcr_models} function,
#' we now predict on the test data to get a glimpse into this model's future out-of-sample performance. 
#' Warning: once this is done, you cannot "go back" and "try" to assess performance on new kernels as this
#' would then be snooping. Run this function when you are ready to close the books on this data set and never
#' look back.
#' 
#' @param explore_kpclr_obj 		This object is built from \code{explore_kplcr_models}. We assume the user has updated 
#' 									this object with a satisfactory model by settings \code{winning_kernel_num} to denote 
#' 									which kernel is selected for the final model and setting \code{winning_rho_num} to 
#' 									denote which proportion of the variance of the kernel matrix is selected for the 
#' 									final model.
#' @param use_validation_data		Should we use the validation data along with the training data. Default is \code{TRUE}.
#' 									From our experience, leaving this \code{FALSE} allows models with better out-of-sample
#' 									error ratios (number of false negatives to false positives or vice versa). The tradeoff
#' 									is a larger overall misclassification error because the model is build with the sample size
#' 									of the training data, not the training plus the validation data.
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
eval_winning_lr_model_on_test_data = function(explore_kpclr_obj, use_validation_data = TRUE){
	#predict the model on test data and build a confusion matrix
	#predict the model on training and validation data
	winning_kernel_info = explore_kpclr_obj$kernel_list[[explore_kpclr_obj$winning_kernel_num]]
	
	if (use_validation_data){
		Xbuild = rbind(explore_kpclr_obj$X_train, explore_kpclr_obj$X_validate)
		ybuild = c(explore_kpclr_obj$y_train, explore_kpclr_obj$y_validate)
	} else {
		Xbuild = explore_kpclr_obj$X_train
		ybuild = explore_kpclr_obj$y_train
	}
	
	kpca = build_kpca_object(Xbuild, winning_kernel_info$kernel_type, winning_kernel_info$params)
	weights = weights_for_kpclr(ybuild, explore_kpclr_obj$fn_cost / explore_kpclr_obj$fp_cost)
	winning_model = kpclr(kpca, ybuild, frac_var = explore_kpclr_obj$rho_seq[explore_kpclr_obj$winning_rho_num], weights = weights, family = explore_kpclr_obj$family)	
	p_test_hat = predict(winning_model, explore_kpclr_obj$X_test)
	test_confusion = table(explore_kpclr_obj$y_test, ifelse(p_test_hat > 0.5, 1, 0)) ###FIX LATER!!!
	#pass back the data
	explore_kpclr_obj$test_confusion = test_confusion
	explore_kpclr_obj$test_confusion_proportions = test_confusion / explore_kpclr_obj$n_test
	explore_kpclr_obj$test_misclassification_error = (test_confusion[1, 2] + test_confusion[2, 1]) / explore_kpclr_obj$n_test
	explore_kpclr_obj$test_weighted_cost = test_confusion[2, 1] * explore_kpclr_obj$fn_cost + test_confusion[1, 2] * explore_kpclr_obj$fp_cost
	explore_kpclr_obj$p_test_hat = p_test_hat
	explore_kpclr_obj
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
#' @param use_validation_data		Should we use the validation data along with the training data. Default is \code{TRUE}.
#' 									From our experience, leaving this \code{FALSE} allows models with better out-of-sample
#' 									error ratios (number of false negatives to false positives or vice versa). The tradeoff
#' 									is a larger overall misclassification error because the model is build with the sample size
#' 									of the training data, not the training plus the validation data.
#' 
#' @return 							An expanded \code{explore_kpcr} list object with new entries
#' 									that contain information about the performance of the final model on the
#' 									test data: \code{L2_err}, the sum of squared error; \code{rmse}, the root 
#' 									mean squared error and \code{L1_err}, the sum of absolute error.
#' 
#' @seealso 						\code{\link{explore_kplr_models}}
#' @author 							Adam Kapelner and Justin Bleich
#' @export
eval_winning_r_model_on_test_data = function(explore_kpcr, use_validation_data = TRUE){
	#predict the model on training and validation data
	winning_kernel_info = explore_kpcr$kernel_list[[explore_kpcr$winning_kernel_num]]
	X_train_and_validate = rbind(explore_kpcr$X_train, explore_kpcr$X_validate)
	kpca = build_kpca_object(X_train_and_validate, winning_kernel_info$kernel_type, winning_kernel_info$params)
	y_train_and_validate = c(explore_kpcr$y_train, explore_kpcr$y_validate)
	winning_model = kpcr(kpca, y_train_and_validate, frac_var = explore_kpcr$rho_seq[explore_kpcr$winning_rho_num])
	y_test_hat = predict(winning_model, explore_kpcr$X_test)
	#pass back the performance data
	sse = sum((y_test_hat - explore_kpcr$y_test)^2)
	explore_kpcr$L2_err = sse
	explore_kpcr$rmse = sqrt(sse / explore_kpcr$n_test)	
	explore_kpcr$L1_err = sum(abs(y_test_hat - explore_kpcr$y_test))
	explore_kpcr
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
		kpclr(kpca, y, frac_var = explore_kpclr_or_kpcr$rho_seq[explore_kpclr_or_kpcr$winning_rho_num], weights = weights, family = explore_kpclr_or_kpcr$family)	
	} else if (class(explore_kpclr_or_kpcr) == "explore_kpclr"){
		kpcr(kpca, y, frac_var = explore_kpclr_or_kpcr$rho_seq[explore_kpclr_or_kpcr$winning_rho_num])
	}	
}