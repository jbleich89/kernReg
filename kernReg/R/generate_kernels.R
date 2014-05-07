##Kernel Functions

k_radial_basis = function(x1, x2, gamma){
  d = x1 - x2
  exp(-gamma * (d %*% d))
}

k_anova_basis = function(x1, x2, params){
  gamma = params[1]
  d = params[2]
  sum(exp(-gamma * (x1 - x2)^2))^d
}

K_matrix = function(X, fun, params){
  X = as.matrix(X)
  n = nrow(X)
  K = matrix(NA, nrow = n, ncol = n)
#   for (i in 1 : n){			
#     K[i, ] = K_vector(X[i, ], X, fun, params)
#   }
  K = sapply(1 : n, function(s) K_vector(X[s,], X, fun, params))
  K_obj = list(K = K, fun = fun, params = params)
  class(K_obj)= "kernel_matrix"
  K_obj
}

K_vector = function(xstar, X, fun, params){
  X = as.matrix(X)
  n = nrow(X)
  K = array(NA, n)
#   for (i in 1 : n){
#     K[i] = fun(xstar, X[i, ], params)
#   }
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
  #mat = matrix(1/m, nrow = m, ncol = m)
  #mat_t = matrix(1/m, nrow = 1, ncol = m)
  #k_vec_c = t(k_vec) - mat_t %*% K  - k_vec %*% mat + mat_t %*% K %*% mat 
  k_vec_c = t(k_vec) - colSums(K)/m  - rep(sum(k_vec)/m , m) + sum(K)/m^2
  k_vec_c
}


