# EM algorithm with multivariate normal
# distribution to estimate cluster assignment

mvnorm.cov.inv <- function(Sigma) {
  # Eigendecomposition of covariance matrix
  E <- eigen(Sigma)
  Lambda.inv <- diag(E$values^-1)   # diagonal matrix
  Q <- E$vectors
  return(Q %*% Lambda.inv %*% t(Q))
}


#multivariate Gaussian pdf
mvn.pdf.i <- function(xi, mu, Sigma)
  1/sqrt( (2*pi)^length(xi) * det(Sigma) ) * 
  exp(-(1/2) * t(xi - mu) %*% mvnorm.cov.inv(Sigma) 
  %*% (xi - mu)  )
mvn.pdf <- function(X, mu, Sigma)
  apply(X, 1, function(xi) mvn.pdf.i(as.numeric(xi), mu, Sigma))
gmm_EM <- function(X, k){
  p <- ncol(X)  # number of parameters
  n <- nrow(X)  # number of observations
  Delta <- 1; iter <- 0; itermax <- 30
  while(Delta > 1e-4 && iter <= itermax){
    # for GMMs it’s very common to first run k-means on your data to get some hard-labels on the data. 
    # With these hard-labels, we can use MLE to estimate the component parameters for our initialization
    if(iter == 0){
      km.init <- kmeans(X, k)
      mu <- km.init$centers; mu_mem <- mu
      w <- sapply(1:k, function(i) length(which(km.init$cluster == i)))
      w <- w/sum(w)
      cov <- array(dim = c(p, p, k))
      for(i in 1:p) for(j in 1:p) for(c in 1:k) cov[i, j, c] <- 
        1/n * sum((X[km.init$cluster == c, i] - mu[c, i]) *
        (X[km.init$cluster == c, j] - mu[c, j]))
    }
    
    # E-step
    mvn.c <- sapply(1:k, function(c) mvn.pdf(X, mu[c,], cov[,, c]))
    r_ic <- t(w*t(mvn.c)) / rowSums(t(w*t(mvn.c)))
    
    # M-step
    n_c <- colSums(r_ic)
    w <- n_c/sum(n_c)
    mu <- t(sapply(1:k, function(c) 1/n_c[c] * colSums(r_ic[, c] *
      X)))
    for(i in 1:p) for(j in 1:p) for(c in 1:k) cov[i, j, c] <-
      1/n_c[c] * sum(r_ic[, c] * (X[, i] - mu[c, i]) * r_ic[, c] *
      (X[, j] - mu[c, j]))
    Delta <- sum((mu - mu_mem)^2)
    iter <- iter + 1; mu_mem <- mu
  }
  return(list(softcluster = r_ic, cluster = apply(r_ic, 1,
    which.max)))
}


# load data
X <- read.csv(file = 'kmeans.csv')
k = 3

# run EM 
gmm <- gmm_EM(X, k)

# pairs plot
pairs(X, col = gmm$cluster)

# cluster assignment
Y <- X
Y$cluster = gmm$cluster
cat(sprintf("\n ---- Cluster assignment ----\n"))
print(Y)

# cluster weights
cat(sprintf("\n ---- Weights ----\n"))
print(gmm$softcluster)