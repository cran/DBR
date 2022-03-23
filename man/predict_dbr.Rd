\name{predict.dbr}
\alias{predict.dbr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict method for Discretized Beta Regression Fits
}
\description{
Predict method for Discretized Beta Regression Fits
}
\usage{
\method{predict}{dbr}(
  object
  , newdata = NULL
  , type = c("sample", "point")
  , ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Standard \code{R} formula describing the response variable and predictors.}
  \item{newdata}{Data frame containing the predictors, matching the \code{dbr} call.}
  \item{type}{Whether to make point predictions or generate sample from the predictive distribution.}
  \item{...}{Further arguments passed to or from other methods.}
}
\value{
If \code{type} is 'point', this will be a vector of same length as \code{nrow(newdata)}, with unique values consistent with the \code{dbr} call. If \code{type} is 'sample', this will be a 2D array of size \code{nrow(data)} x \code{nsmp}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

\dontrun{

library("DBR")

data("pain")
est <- dbr(
  interference ~ severity + age
  , pain
  , control = dbr.control(
    nsmp = 200
    , nburnin = 100
  )
)

predict(est, type = "point")

}

}
