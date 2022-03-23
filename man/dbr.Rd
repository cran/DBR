\name{dbr}
\alias{dbr}
\alias{dbr.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discretized Beta Regression for Survey-Response Analysis
}
\description{
Discretized Beta Regression for Survey-Response Analysis
}
\usage{
dbr(
  formula
  , data
  , control = dbr.control()
  , yunique = NULL
  , wghts = rep(1, nrow(data))
)
dbr.control(
  nsmp = 100
  , nburnin = 50
  , estimate_left_buffer = FALSE
  , estimate_right_buffer = FALSE
  , buffer_max = 5.0
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Standard \code{R} formula describing the response variable and predictors.}
  \item{data}{Data frame containing response and predictors as described in \code{formula}.}
  \item{control}{List of parameters for controlling the MCMC estimation.}
  \item{yunique}{Vector of values/levels that the response variable can assume. If not specified, this will be extracted from the \code{data} according to the \code{formula}.}
  \item{wghts}{Vector of weights to be applied during model estimation. Default is a uniform weight vector.}
  \item{nsmp}{Number of MCMC samples to collect, including the burnin phase.}
  \item{nburnin}{Number of initial MCMC samples to discard before calculating parameter estimates.}
  \item{estimate_left_buffer}{Boolean flag indicating whether to estimate a left buffer from the data.}
  \item{estimate_right_buffer}{Boolean flag indicating whether to estimate a right buffer from the data.}
  \item{buffer_max}{Maximum size of left/right buffer, only used if above flags are set to TRUE.}
}
\value{
An object of class \code{dbr}, which is a list containing the following fields:
  \item{formula}{Same as input.}
  \item{control}{Same as input.}
  \item{yunique}{Same as input.}
  \item{wghts}{Same as input.}
  \item{est}{An internal object containing estimation results. Should not be accessed directly by user. Use \code{summary} and \code{predict} instead.}
  \item{data}{Same as input.}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{summary.dbr}}, \code{\link{predict.dbr}}
}

\examples{

library("DBR")
data("pain")
est <- dbr(
  interference ~ severity + age
  , pain
  , control = dbr.control(
    nsmp = 50
    , nburnin = 25
  )
)

}
