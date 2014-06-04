#' Number of PC's from a fraction
#' 
#' A helper method to get the number of PC's from the percentage of variation we wish to explain
#' as a proportion.
#' 
#' @param kpca_object				The Kernel PCA object 
#' @param frac_var_to_explain 		The proportion of variance we wish to explain
#' @return 							The number of PC's needed
#' 
#' @author 							Justin Bleich and Adam Kapelner 
get_num_pcs_from_frac = function(kpca_object, frac_var_to_explain){
	sum(cumsum(kpca_object$keigenvals / sum(kpca_object$keigenvals)) <= frac_var_to_explain) + 1
}