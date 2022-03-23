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
