#ptim_share <- function(par, fngr, ...) {
#  fn <- grad_share(fngr)
#  optim(par=par, fn=fn, gr=function(xx) {grad_store}, ...)
#}
optim_share <- function(par, fngr, ...) {
  env <- grad_share(fngr)
  optim(par=par, fn=env$fn, gr=env$gr, ...)
}
#optim_share(par=c(3, -5), quad_share, method="BFGS")

#lbfgs_share <- function(fngr, vars, method=NULL,...) {
#  fn <- grad_share(fngr)
#  lbfgs::lbfgs(call_eval=fn, call_grad=function(xx) {grad_store}, vars=vars, ...)
#}

lbfgs_share <- function(fngr, vars, method=NULL,...) {
  env <- grad_share(fngr)
  lbfgs::lbfgs(call_eval=env$fn, call_grad=env$gr, vars=vars, ...)
}
#lbfgs_share(vars=c(3, -5), fngr=quad_share)
#parallel::mclapply(1:10, function(i)lbfgs_share(vars=runif(2,-10,10), fngr=quad_share, invisible=1), mc.cores=1)
