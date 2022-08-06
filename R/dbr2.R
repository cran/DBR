predict.dbr <- function(
  object
  , newdata = NULL
  , type = c("sample", "point")
  , ...) {
  type <- match.arg(type)
  
  if (is.null(newdata)) newdata <- object$data
  predict(object$est, newdata = newdata, type = type
          , ...)
}

print.dbr <- function(x, ...) {
  cat("formula:\n")
  print(x$formula)
}

plot.dbr <- function(x, ...) {
  est <- x$est@learners[[2]]@est
  plot(est$smp, ask = FALSE)
  plot(est$loglike, type = "l", xlab = "iteration", ylab = "log-like")
  
  return (NULL)
}

coef.dbr <- function(
  object
  , prob = c(0.025, 0.5, 0.975)
  , ...) {
  est <- object$est@learners[[2]]@est
  nsmp <- object$control$nsmp
  nburnin <- object$control$nburnin
  ret <- apply(est$smp[nburnin + 1:(nsmp - nburnin), ], 2, quantile, prob = prob)
  return(ret)
}

summary.dbr <- function(object, context, make_plot = TRUE, ...) {
  if (missing(context)) {
    context <- object$data[1, ]
  }
  
  # get list of unique predictors and their classes
  response <- all.vars(object$formula)[1]
  unique_predictors <- all.vars(object$formula)[-1]
  predictor_classes <- sapply(unique_predictors, function(x) {
    class(object$data[[x]])
  })
  nPred <- length(unique_predictors)
  
  # for each unique predictor:
  # create vector of values --> prediction dataframe
  # plot results
  # logic for creating vector of values:
  # numeric: use min/max from training data, resolution from default
  # integer: use min/max from training data, use increment of 1
  # logical: T/F
  # factor: training data, get levels
  ret <- lapply(1:nPred, function(n) {
    my_predictor <- unique_predictors[n]
    my_class <- predictor_classes[n]
    x_data <- object$data[, my_predictor]
    if (my_class == "numeric") {
      xvec <- seq(from = min(x_data), to = max(x_data), length.out = 10)
    } else if (my_class == "integer") {
      xvec <- seq(from = min(x_data), to = max(x_data), by = 1)
    } else if (my_class == "logical") {
      xvec <- c(FALSE, TRUE)
    } else if (my_class == "factor") {
      warning("support for factor predictors in 'coef' coming soon")
      xvec <- NULL
    } else {
      warning("unexpected predictor class: ", my_class)
      xvec <- NULL
    }
    
    nx <- length(xvec)
    if (nx > 0) {
      predDF <- context[rep(1, nx), unique_predictors]
      predDF[, my_predictor] <- xvec
      yvec <- predict(object = object, newdata = predDF, type = "point")
      if (make_plot) {
        plot(xvec, yvec
             , main = paste0("mean predicted ", response, " vs. ", my_predictor)
             , ylab = response
             , xlab = my_predictor, type = "l", pch = 4
             , ylim = range(object$yunique))
      }
      return (list(X = predDF, y = yvec))
    } else {
      return (NULL)
    }
    
  })
  
  names(ret) <- unique_predictors
  
  ret
  
}

