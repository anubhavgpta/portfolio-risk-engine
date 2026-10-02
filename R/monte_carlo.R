simulate_portfolio <- function(returns, weights, n = 10000L, seed = 42L) {
  if (!is.finite(n) || n < 10000 || n > 200000 || n != as.integer(n))
    stop("Choose between 10,000 and 200,000 simulations.")
  x <- as.matrix(returns)
  if (nrow(x) < 2 || any(!is.finite(x))) stop("Finite aligned returns are required.")
  # An eigen square root handles singular covariance (identical assets) as well.
  eig <- eigen(stats::cov(x), symmetric = TRUE)
  root <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = ncol(x))
  withr::with_seed(seed, {
    z <- matrix(rnorm(n * ncol(x)), nrow = n)
    simulated <- sweep(z %*% t(root), 2, colMeans(x), "+")
    colnames(simulated) <- colnames(x)
    portfolio_returns(simulated, weights)
  })
}

