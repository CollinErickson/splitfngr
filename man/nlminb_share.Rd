% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimization.R
\name{nlminb_share}
\alias{nlminb_share}
\title{Use splitfngr with nlminb}
\usage{
nlminb_share(start, fngr, ...)
}
\arguments{
\item{start}{Initial values for the parameters to be optimized over.
Will be passed to nlminb as start argument.}

\item{fngr}{A function that returns a list of two elements:
the function value and the gradient value.}

\item{...}{Other arguments passed to nlminb}
}
\value{
Result from running nlminb on the given function
}
\description{
Use nlminb function but pass in a single function
that returns both the function and gradient
together in a list. Useful when the function and
gradient are expensive to calculate and can be
calculated faster together than separate.
}
\examples{
quad_share <- function(x){list(sum(x^4), 4*x^3)}
nlminb_share(start=c(3, -5), fngr=quad_share)

\dontrun{
# Add a sleep amount to show when it can be faster

# Using share
quad_fngr <- function(x){Sys.sleep(.01); list(sum(x^4), 4*x^3)}
system.time(nlminb_share(start=c(3, -5), fngr=quad_fngr))

# Without share
quad_fn <- function(x) {Sys.sleep(.01); sum(x^4)}
quad_gr <- function(x) {Sys.sleep(.01); 4*x^3}
system.time(nlminb(c(3,-5), quad_fn, quad_gr))
}
}
