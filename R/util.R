coda_wrapper <- function(dbr_obj, coda_function, ...) {
  smp <- dbr_obj$est@learners[[2]]@est$smp
  estimate_left_buffer <- dbr_obj$control$estimate_left_buffer
  estimate_right_buffer <- dbr_obj$control$estimate_right_buffer
  smp_ncol <- ncol(smp)
  idx <- 3:smp_ncol
  if (estimate_right_buffer) idx <- c(2, idx)
  if (estimate_left_buffer) idx <- c(1, idx)
  
  mcmc_obj <- mcmc(
    data = smp[, idx]
    , start = dbr_obj$control$nburnin
    , end = dbr_obj$control$nsmp
    , thin = 1)
  coda_function(mcmc_obj, ...)
}

get_intervals <- function(uniqueVals) {
  uniqueVals <- sort(uniqueVals)
  0.5 * (uniqueVals[1:(length(uniqueVals) - 1)] + uniqueVals[-1])
}

meanPrecision_to_alphaBeta <- function(mu, precision) {
  alpha <- precision * mu
  beta <- precision * (1 - mu)
  return(cbind(alpha, beta))
}

beta_to_discrete <- function(ybeta, uniqueVals, discretise = T, ...) {
  uniqueVals <- sort(uniqueVals)
  
  my_shift <- get_shift(uniqueVals)
  my_scale <- get_scale(uniqueVals)
  
  ret_raw <- my_scale * ybeta - my_shift
  
  if (discretise) {
    # brackets after ybeta in assignment are used to preserve the dimensions of ybeta
    # this is useful when running beta regression prediction in sampling mode
    ybeta[] <- vapply(ret_raw, function(x) {
      uniqueVals[which.min(abs(x - uniqueVals))]
    }, numeric(1))
    ybeta
  } else {
    ret_raw
  }
}

get_shift <- function(xunique) {
  xunique <- sort(xunique)
  
  diff(xunique[1:2]) / 2 - xunique[1]
}
get_scale <- function(xunique) {
  xunique <- sort(xunique)
  
  diff(range(xunique)) + 0.5 * (diff(xunique[1:2]) - diff((rev(xunique))[1:2]))
}
