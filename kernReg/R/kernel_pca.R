
kernel_pca = function(K_object){
  if(class(K_object) != "kernel_matrix") stop("Need a kernel matrix object. Call K_matrix().")
  Kc = center_kernel_matrix(K_object$K)
  keigen = eigen(Kc/nrow(Kc), symmetric = TRUE)
  num_evals = sum(keigen$values > 1e-4) ##numeric stability issue
  keigenvecs = t(t(keigen$vectors[,1:num_evals])/sqrt(keigen$values[1:num_evals]))
  rotated_dat = Kc %*% keigenvecs
  obj = list(eigenvals = keigen$values[1 : num_evals], eigenvecs = keigenvecs, pc_mat = rotated_dat, K_object = K_object)
  class(obj) = "kernel_pca"
  obj
}


get_num_pcs = function(kpca_object, frac_var_to_explain){
  num_pcs = sum(cumsum(kpca_object$eigenvals/sum(kpca_object$eigenvals)) <= frac_var_to_explain) + 1
  num_pcs
}
