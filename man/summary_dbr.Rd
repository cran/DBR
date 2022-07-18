\name{summary.dbr}
\alias{summary.dbr}
\alias{print.dbr}
\alias{coef.dbr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary, print and coef methods for Discretised Beta Regression Fits
}
\description{
Summary, print and coef methods for Discretised Beta Regression Fits
}
\usage{
\method{summary}{dbr}(
  object
  , prob = c(0.025, 0.5, 0.975)
  , make_plot = TRUE
  , ...
)
\method{print}{dbr}(
  x
  , make_plot = FALSE
  , ...
)
\method{coef}{dbr}(
  object
  , context
  , make_plot = TRUE
  , ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Result of call to \code{dbr}.}
  \item{x}{Result of call to \code{dbr}.}
  \item{prob}{Vector of probabilities at which to calculate quantiles of model parameters.}
  \item{make_plot}{Whether to generate MCMC diagnostic plots.}
  \item{context}{Dataframe with a single row, containing all columns needed for prediction. These values are used for producing pseudo-coefficient plots in the \code{coef} method.}
  \item{...}{Further arguments passed to the functions.}
}
\value{
For \code{summary}, a 2D array of size \code{length(prob)} x \code{nVar}, where \code{nVar} is the number of model parameters. The latter itself is composed of left and right buffers, precision parameter of beta distribution, and covariate coefficients. Note that if left and/or right buffers were not estimated by \code{dbr}, their confidence interval will be identical to the point estimate. The \code{print} method prints the formula used to call \code{dbr}, along with the output of \code{summary}.
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
summary(est, make_plot = FALSE, prob = c(0.05, 0.5, 0.95))
print(est)
coef(est, comtext = pain[2, ])

}
