##Kernel Functions

k_radial_basis = function(x1, x2, params = 1){
  d = x1 - x2
  exp(-gamma * (d %*% d))
}

k_anova_basis = function(x1, x2, params = c(1, 2)){
  gamma = params[1]
  d = params[2]
  sum(exp(-gamma * (x1 - x2)^2))^d
}

k_polynomial_basis = function(x1, x2, params = c(2,1,1)){
  degree = params[1]
  scale = params[2]
  offset = params[3]
  (scale*crossprod(x1, x2) + offset)^degree
}


k_vanilla_basis = function(x1, x2, params = NULL){
  crossprod(x1, x2)
}

K_matrix = function(X, fun, params){
  if(class(X) != "matrix") stop("X must be a matrix")
  X = as.matrix(X)
  n = nrow(X)
  K = matrix(NA, nrow = n, ncol = n)
  K = sapply(1 : n, function(s) K_vector(X[s,], X, fun, params))
  K_obj = list(K = K, fun = fun, params = params)
  class(K_obj)= "kernel_matrix"
  K_obj
}

K_vector = function(xstar, X, fun, params){
  X = as.matrix(X)
  n = nrow(X)
  K = array(NA, n)
  K = sapply(1 : n, function(s) fun(xstar, X[s,], params))
  K
}

center_kernel_matrix = function(K){
  m = dim(K)[1]
  Kc = t(t(K - colSums(K)/m) -  rowSums(K)/m) + sum(K)/m^2
  Kc
}

center_kernel_test_vec = function(k_vec, K){
  m = length(k_vec)
  k_vec_c = t(k_vec) - colSums(K)/m  - rep(sum(k_vec)/m , m) + sum(K)/m^2
  k_vec_c
}

