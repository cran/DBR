setGeneric("train", function(object, ...) standardGeneric("train"))
setClass("Learner", slots = c(est = "ANY", pred = "ANY"), contains = "VIRTUAL")
setClass("Template", slots = c(learners = "list"), contains = "Learner")

#### templates ####

## pipeline ##
Pipeline <- setClass(
  "Pipeline"
  , contains = "Template"
)
setMethod(
  "train", "Pipeline"
  , function(object, formula, data, ...) {
    object@learners[[1]] <- train(object@learners[[1]], formula, data, ...)
    #browser()
    object@learners[[2]] <- train(object@learners[[2]], predict(object@learners[[1]])[[1]], predict(object@learners[[1]])[[2]], ...)
    object@pred <- predict(object@learners[[2]])
    
    return (object)
  }
)
predict.Pipeline <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return (object@pred)
  return (
    {
      predict(object@learners[[2]], predict(object@learners[[1]], newdata, ...)[[1]], ...)
    }
  )
}

#### learners ####

## model frame ##
ModelFrame <- setClass(
  "ModelFrame"
  , contains = "Learner"
  #, slots = c()
  #, validity = function(object) {
  #  TRUE
  #}
)
setMethod(
  "train", "ModelFrame"
  , function(object, formula, data, ...) {
    mf <- model.frame(formula, data, drop.unused.levels = TRUE, na.action = na.fail)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)
    y <- model.response(mf, "numeric")
    
    
    object@pred <- list(X = X, y = y)
    object@est <- list(
      terms = mt
      , xlevels = .getXlevels(mt, mf)
      , contrasts = attr(X, "contrasts")
    )
    
    return (object)
  }
)
predict.ModelFrame <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return (
    object@pred
  )
  
  # need (terms, xlevels, contrasts) in object@est
  terms <- delete.response(object@est$terms)
  newdata <- droplevels(newdata)
  mf <- model.frame(terms, newdata, xlev = object@est$xlevels)
  X <- model.matrix(terms, mf, contrasts.arg = object@est$contrasts)
  
  return (list(X = X))
}

# modified (Bayesian) beta regression
ModifiedBetaRegression <- setClass(
  "ModifiedBetaRegression"
  , contains = "Learner"
  , slots = c(
    nsmp = "integer"
    , nburnin = "integer"
    , estimate_left_buffer = "logical"
    , estimate_right_buffer = "logical"
    , buffer_max = "numeric"
    , yunique = "ANY"
  )
  , prototype = list(
    nsmp = 100L
    , nburnin = 50L
    , estimate_left_buffer = FALSE
    , estimate_right_buffer = FALSE
    , buffer_max = 5.0
    , yunique = NULL
  )
)

setMethod(
  "train", "ModifiedBetaRegression"
  , function(object, X, y, wghts = rep(1, NROW(X)), ...) {
    
    loglike <- function(
      params, X
      , yLeft, yRight
      , yMin, yMax
      , wghts = rep(1, NROW(X))
    ) {
      #browser()
      
      # params: 1- left buffer, 2- right buffer
      # 3- beta precision param (log link), 4- coefficients of beta mean param (logit link)
      buffer.left <- params[1]
      buffer.right <- params[2]
      
      
      # calculating shape parameters of beta distribution
      precision <- exp(params[3])
      mean.vec <- 1 / (1 + exp(-1.0 * X %*% params[-(1:3)]))
      shapes <- meanPrecision_to_alphaBeta(mu = mean.vec, precision = precision)
      
      # creating left and right boundaries for observations
      yMin.expanded <- yMin - buffer.left
      yMax.expanded <- yMax + buffer.right
      
      idx.left <- is.na(yLeft)
      #stopifnot(length(which(idx.left)) == 0)
      yLeft[idx.left] <- yMin.expanded
      
      idx.right <- is.na(yRight)
      #stopifnot(length(which(idx.right)) == 0)
      yRight[idx.right] <- yMax.expanded
      
      # mapping the boundaries to [0,1]
      yRange.expanded <- yMax.expanded - yMin.expanded
      yLeft.rescaled <- (yLeft - yMin.expanded) / yRange.expanded
      yRight.rescaled <- (yRight - yMin.expanded) / yRange.expanded
      
      #browser()
      
      sum(
        wghts * log(
          pbeta(q = yRight.rescaled, shape1 = shapes[, 1], shape2 = shapes[, 2], log.p = F) -
            pbeta(q = yLeft.rescaled, shape1 = shapes[, 1], shape2 = shapes[, 2], log.p = F)
        )
      )
    }
    
    nVar <- ncol(X) + 1
    nsmp <- object@nsmp
    nburnin <- object@nburnin
    estimate_left_buffer <- object@estimate_left_buffer
    estimate_right_buffer <- object@estimate_right_buffer
    buffer_max <- object@buffer_max
    
    yunique <- object@yunique
    if (is.null(yunique)) {
      yunique <- sort(unique(y))
    } else {
      stopifnot(min(yunique) <= min(y) && max(yunique) >= max(y))
    }
    
    #browser()
    yintervals <- get_intervals(yunique)
    my_intervals <- findInterval(x = y, vec = yintervals) # could this be sensitive to floating-point errors?
    
    is.extreme_left <- (my_intervals == 0)
    is.extreme_right <- (my_intervals >= length(yintervals))
    
    yLeft <- yRight <- NA * y
    yLeft[!is.extreme_left] <- yintervals[my_intervals[!is.extreme_left]]
    yRight[!is.extreme_right] <- yintervals[my_intervals[!is.extreme_right] + 1]
    
    if (!estimate_left_buffer) {
      yLeft[is.extreme_left] <- yunique[1] - 
        0.5 * (yunique[2] - yunique[1])
    }
    
    if (!estimate_right_buffer) {
      yRight[is.extreme_right] <- yunique[length(yunique)] + 
        0.5 * (yunique[length(yunique)] - yunique[length(yunique) - 1])
    }
    
    yMin <- min(yunique)
    yMax <- max(yunique)
    
    #browser()
    
    smp <- MfU.Sample.Run(
      x = c(rep(0.1, 2), rep(0.0, nVar))
      , f = loglike
      , uni.sampler = "slice"
      , control = MfU.Control(
        n = nVar + 2
        , slice.lower = c(rep(1e-6, 2), rep(-Inf, nVar))
        , slice.upper = c(rep(buffer_max, 2), rep(+Inf, nVar))
        , slice.m = 10
      )
      , X = X
      , yLeft = yLeft
      , yRight = yRight
      , yMin = yMin
      , yMax = yMax
      , wghts = wghts
      , nsmp = nsmp
    )
    
    loglike_smp <- sapply(1:nsmp, function(n) {
      loglike(
        params = smp[n, ]
        , X = X
        , yLeft = yLeft
        , yRight = yRight
        , yMin = yMin
        , yMax = yMax
        , wghts = wghts
      )
    })
    
    if (!estimate_left_buffer) {
      smp[, 1] <- 0.5 * (yunique[2] - yunique[1])
    }
    if (!estimate_right_buffer) {
      smp[, 2] <- 0.5 * (yunique[length(yunique)] - yunique[length(yunique) - 1])
    }
    
    colNames_X <- if (is.null(colnames(X))) paste0("X", 1:(nVar - 1)) else colnames(X)
    
    colnames(smp) <- c("left_buffer", "right_buffer", "precision", colNames_X)
    
    object@est <- list(
      smp = smp
      , yMin = yMin
      , yMax = yMax
      , yLeft = yLeft
      , yRight = yRight
      , uniqueVals = yunique
      , loglike = loglike_smp
    )
    
    object@pred <- NULL # TODO: change this
    
    return (object)
  }
)

predict.ModifiedBetaRegression.core <- function(params, X, yMin, yMax, type = "point") {
  
  buffer.left <- params[1]
  buffer.right <- params[2]
  precision <- exp(params[3])
  #browser()
  mean.vec <- 1 / (1 + exp(-1.0 * X %*% params[-(1:3)]))
  
  yraw <- if (type == "point") {
    mean.vec
  } else {
    shapes <- meanPrecision_to_alphaBeta(mu = mean.vec, precision = precision)
    rbeta(n = nrow(X), shape1 = shapes[, 1], shape2 = shapes[, 2])
  }
  
  yMin.expanded <- yMin - buffer.left
  yMax.expanded <- yMax + buffer.right
  yRange.expanded <- yMax.expanded - yMin.expanded
  
  yraw * yRange.expanded + yMin.expanded
}

predict.ModifiedBetaRegression <- function(
  object
  , newX = NULL
  , type = c("point", "sample")
  #, nsmp = 50
  #, discretise = T
  , ...
) {
  if (is.null(newX)) return (object@pred)
  
  type <- match.arg(type)
  
  nsmp.mcmc <- object@nsmp
  nburnin <- object@nburnin
  smp <- object@est$smp
  yMin <- object@est$yMin
  yMax <- object@est$yMax
  uniqueVals <- object@est$uniqueVals
  
  #stopifnot((type == "point") || (nsmp <= nsmp.mcmc - nburnin))
  
  ret.continuous <- sapply(1:nsmp.mcmc, function(n) {
    predict.ModifiedBetaRegression.core(params = smp[n, ], X = newX, yMin = yMin, yMax = yMax, type = type)
  })
  #browser()
  ret.continuous <- matrix(ret.continuous, ncol = nsmp.mcmc)
  
  #browser()
  
  #if (type == "sample") {
  if (TRUE) {
    ret.continuous[] <- vapply(ret.continuous, function(x) {
      uniqueVals[which.min(abs(x - uniqueVals))]
    }, numeric(1))
  }
  
  #if (type == "point") {
  #  ret.continuous <- rowMeans(ret.continuous[, (nburnin + 1):nsmp.mcmc])
  #}
  
  #browser()
  
  if (type == "sample") return (ret.continuous[, (nburnin + 1):nsmp.mcmc, drop = FALSE])
  else return (rowMeans(ret.continuous[, (nburnin + 1):nsmp.mcmc, drop = FALSE]))
}

dbr.control <- function(
  nsmp = 100
  , nburnin = 50
  , estimate_left_buffer = FALSE
  , estimate_right_buffer = FALSE
  , buffer_max = 5.0 # TODO: replace static value with with a dynamic (data-driven) rule
) {
  list(
    nsmp = nsmp
    , nburnin = nburnin
    , estimate_left_buffer = estimate_left_buffer
    , estimate_right_buffer = estimate_right_buffer
    , buffer_max = buffer_max
  )
}

dbr <- function(
  formula
  , data
  , control = dbr.control()
  , yunique = NULL
  , wghts = rep(1, nrow(data))
) {
  obj <- Pipeline(learners = list(
      ModelFrame()
      , ModifiedBetaRegression(
        nsmp = as.integer(control$nsmp)
        , nburnin = as.integer(control$nburnin)
        , estimate_left_buffer = as.logical(control$estimate_left_buffer)
        , estimate_right_buffer = as.logical(control$estimate_right_buffer)
        , buffer_max = control$buffer_max
        , yunique = yunique
      )
    ))

  obj <- train(obj, formula, data, y_unique = yunique, wghts = wghts)
  
  ret <- list(
    formula = formula
    , control = control
    , yunique = obj@learners[[2]]@est$uniqueVals
    , wghts = wghts
    , est = obj
    , data = data
  )
  class(ret) <- c("dbr", class(ret))
  
  return (ret)
  
}

predict.dbr <- function(
  object
  , newdata = NULL
  , type = c("sample", "point")
  #, nsmp = object$control$nsmp - object$control$nburnin
  #, discretise = F
  , ...) {
  type <- match.arg(type)
  
  #if (is.null(newdata)) return (object$est@pred)
  if (is.null(newdata)) newdata <- object$data
  predict(object$est, newdata = newdata, type = type
          #, nsmp = nsmp, discretise = discretise
          , ...)
}

summary.dbr <- function(
  object
  , prob = c(0.025, 0.5, 0.975)
  , make_plot = TRUE
  , ...
  ) {
  est <- object$est@learners[[2]]@est
  
  if (make_plot) {
    plot(est$smp, ask = FALSE)
    plot(est$loglike, type = "l", xlab = "iteration", ylab = "log-like")
  }
  
  nsmp <- object$control$nsmp
  nburnin <- object$control$nburnin
  ret <- apply(est$smp[nburnin + 1:(nsmp - nburnin), ], 2, quantile, prob = prob)
  
  return(ret)
}

print.dbr <- function(x, make_plot = FALSE, ...) {
  cat("formula:\n")
  print(x$formula)
  cat("coefficient estimates:\n")
  print(summary(x, make_plot = make_plot, ...))
}

