# A private helper function which checks with an object is of a certain type 
# and if not, crashes the script and prints an appropriate error message
# 
# @param obj						The object to check 
# @param var_name 					The name of the original variable name that is being checked
# @param class_types 				The class name(s) \code{obj} should be
# @param class_builder_functions 	The name(s) of the function used that builds the correct class type. Defaults to NULL.
# 
# @author Adam Kapelner
checkObjectType = function(obj, var_name, class_types, class_builder_functions = NULL){
	if (!(is(obj, class_types))){
		if (is.null(class_builder_functions)){
			stop(var_name, " must be an object of type \"", paste(class_types, collapse = "\" or \""), ".\"", sep = "")
		} else {
			stop(var_name, " must be an object of type \"", paste(class_types, collapse = "\" or \""), 
				".\" Call ", paste(class_builder_functions, collapse = "() or ") , "().", sep = "")	
		}
	}
}