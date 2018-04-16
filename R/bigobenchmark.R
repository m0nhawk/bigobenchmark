#' bigobenchmark runner
#'
#' @param ... Expressions to benchmark. Passed to \code{microbenchmark}.
#' @param args List of arguments to run functions on.
#' @param list List of unevaluated expression to benchmark. Passed to \code{microbenchmark}.
#' @param times Number of times to evaluate the expression. Passed to \code{microbenchmark}.
#' @param check Function to check if the expressions are equal. By default \code{NULL} which omits the check. Passed to \code{microbenchmark}.
#' @param control List of control arguments. See Details section for \code{microbenchmark}. Passed to \code{microbenchmark}.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @author Andrew Prokhorenkov
bigobenchmark <- function(...,
                          args = NULL,
                          list=NULL,
                          times=100L,
                          check=NULL,
                          control=list()) {
  exprs <- c(as.list(match.call(expand.dots = FALSE)$`...`), list)
  r <- vector("list", length(args))
  for(i in 1:length(args)) {
    n <- args[[i]]
    r[[i]] <- summary(do.call(microbenchmark::microbenchmark, c(exprs, list=list, times=times, check=check, control=control)))
    r[[i]]["arg"] <- n
  }
  bind_rows(r)
}
