% Generated by roxygen2 (4.0.1): do not edit by hand
\name{predict.kpcr}
\alias{predict.kpcr}
\title{Predicts for new data}
\usage{
\method{predict}{kpcr}(object, new_data, num_cores = 1, ...)
}
\arguments{
\item{object}{The Kernel PCA linear model object used to predict}

\item{new_data}{The new data the user wishes to predict}

\item{num_cores}{Number of cores for parallel prediction}

\item{...}{Other parameters to be passed to \code{predict.lm}}
}
\value{
A vector of predictions with lenth of the number of rows of \code{new_data} generated via \code{predict.lm}
}
\description{
\code{predict.kpcr} predicts using the kernel PCA model for new data
}
\examples{
\dontrun{
#pull the predictor matrix and response from the Boston Housing Data
data(Boston)
y = Boston$medv
Boston$medv = NULL
X = as.matrix(Boston)
#build a KPCA object using the anova kernel with hyperparameters gamma = 0.1 and d = 3
kpca_obj = build_kpca_object(X, "anova", c(0.1, 3))
#build a kpcr model using 75\% of the variance in the kernel matrix
kpcr_mod = kpcr(kpca_obj, y, frac_var = 0.75)
#forecast on the "new" data which here will just be the first 10 rows of the Boston Housing Data
x_star = X[1 : 10, ]
y_hat = predict(kpcr_mod, x_star)
}
}
\author{
Justin Bleich and Adam Kapelner
}
\seealso{
\code{\link{predict.lm}}
}

