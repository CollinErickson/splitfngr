#' Calculate function and gradient together but access separately.
#' Reduces computation since they share data in calculation.
#' Doesn't have to be function and gradient, can be any two
#' values calculated together but accessed separately.
#' Useful in optimization when function evaluation is expensive
#' since the chain rule means many parts of function and gradient
#' are the same.
#'
#' @param fn_gr A function that returns a list of two values.
#' Both are calculated when fn is called, but only the first
#' is returned. The second is returned when gr is called
#' but nothing is recalculated.
#'
#' @return An environment with two functions, fn and gr.
#' @export
#'
#' @examples
#' quad_share <- function(x){list(sum(x^4), 4*x^3)}
#' share <- grad_share(quad_share)
#' share$fn(1)
#' share$gr(1)
#' share$gr(2)
#' share$fn(2)
#' share$gr(2)
grad_share <- function(fn_gr) {
  env <- new.env()
  env$fn <- function(x) {
    out <- fn_gr(x)
    env$x_last <- x
    env$fn_val <- out[[1]]
    env$gr_val <- out[[2]]
    env$fn_val
  }
  env$gr <- function(x = NULL) {
    # Can check if evaluated at same value, but will only slow it down
    #if (!is.null(x) && !any(is.nan(x)) && x != env$x_last) {warning("gr called at different x than fn")}
    env$gr_val
  }
  env
}


#' Access a list of values separately but calculate them together.
#'
#'
#' @param func Function that returns a list of values
#' @param check_all Should it check that the accessed values were
#' calculated at the current input?
#' @param recalculate_indices Indices for which the values should
#' be recalculated.
#'
#' @return
#' @export
#'
#' @examples
#' tfunc <- function(x) {list(x, x+1, x+2, x+3, x+4)}
#' tenv <- fngr(tfunc)
#' tenv(1)(0)
#' tenv(3)(0)
#' tenv(3)(1)
#' tenv(1)(23.4)
#' tenv(4)()
fngr <- function(func, check_all=FALSE, recalculate_indices = 1) {
  env <- new.env()
  env$f <- function(i, check=check_all, recalculate = any(i==recalculate_indices)) {
    function(x=NULL, check_now=check, recalculate_now=recalculate) {
      if (recalculate_now) {
        out <- func(x)
        env$x_last <- x
        env$out <- out
        out[[1]]
      } else {
        # Can check if evaluated at same value, but will only slow it down
        if (check_now) {
          if (!is.null(x) && !any(is.nan(x)) && x != env$x_last) {
            warning("gr called at different x than fn")
          }
        }
      }
      env$out[[i]]
    }
  }
  env
}


make_share <- function(func, arg_fn, arg_gr) {
  function(fngr, ...) {
    env <- grad_share(fngr)
    args_list <- list(env$fn, env$gr, ...)
    names(args_list)[1] <- arg_fn
    names(args_list)[2] <- arg_gr
    do.call(what=func, args=args_list)
  }
}
# make_share(lbfgs::lbfgs, 'call_eval', 'call_grad')
# make_share(lbfgs::lbfgs, 'call_eval', 'call_grad')(quad_share, vars=c(5,-4))
