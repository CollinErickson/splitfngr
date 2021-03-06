% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimization.R
\name{lbfgs_share}
\alias{lbfgs_share}
\title{Use splitfngr with lbfgs}
\usage{
lbfgs_share(fngr, vars, ...)
}
\arguments{
\item{fngr}{A function that returns a list of two elements:
the function value and the gradient value.}

\item{vars}{Initial values for the parameters to be optimized over.
Will be passed to lbfgs as vars argument.}

\item{...}{Other arguments passed to lbfgs}
}
\value{
Result from running lbfgs on the given function
}
\description{
Use lbfgs function from the lbfgs package but pass in a single function
that returns both the function and gradient
together in a list. Useful when the function and
gradient are expensive to calculate and can be
calculated faster together than separate.
}
\examples{
quad_share <- function(x){list(sum(x^4), 4*x^3)}
lbfgs_share(vars=c(3, -5), fngr=quad_share)
}
