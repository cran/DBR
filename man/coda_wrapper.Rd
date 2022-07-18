\name{coda_wrapper}
\alias{coda_wrapper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility function to call MCMC diagnostic functions in the coda package
}
\description{
Utility function to call MCMC diagnostic functions in the coda package
}
\usage{
coda_wrapper(dbr_obj, coda_function, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbr_obj}{An object of class \code{dbr}, typically the output of the function \code{dbr}.}
  \item{coda_function}{A function in the \pkg{coda} package, with first argument being an \code{mcmc} object.}
  \item{...}{Arguments to be passed to \code{coda_function}.}
}
\value{
The output of \code{coda_function}.
}
\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\examples{
library("DBR")
library("coda")
data("pain")
est <- dbr(
  interference ~ severity + age
  , pain
  , control = dbr.control(
    nsmp = 50
    , nburnin = 25
  )
)
coda_wrapper(est, autocorr, lag = c(0, 5))
}
