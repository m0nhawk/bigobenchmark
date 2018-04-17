#' @export
is.bigobenchmark <- function(x) {
  inherits(x, "bigobenchmark")
}

#' bigobenchmark runner
#'
#' @importFrom dplyr bind_rows
#'
#' @param ... Expressions to benchmark. Passed to \code{microbenchmark}.
#' @param args List of arguments to run functions on.
#' @param list List of unevaluated expression to benchmark. Passed to \code{microbenchmark}.
#' @param times Number of times to evaluate the expression. Passed to \code{microbenchmark}.
#' @param check Function to check if the expressions are equal. By default \code{NULL} which omits the check. Passed to \code{microbenchmark}.
#' @param control List of control arguments. See Details section for \code{microbenchmark}. Passed to \code{microbenchmark}.
#'
#' @return An object of class \code{bigobenchmark}.
#' @export
#'
#' @examples
#' # Measure difference between linear and quadratic functions
#' bench <- bigobenchmark(1:n, for(i in 1:n) for(i in 1:n) 1:n, args=seq(from=1, to=100, length.out = 50))
#' bench
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
  return(structure(list(benchmarks=bind_rows(r)), class = "bigobenchmark"))
}

#' Autoplot for bigobenchmark object
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_pointrange
#'
#' @param object
#' @name autoplot
#' @export autoplot.bigobenchmark
#'
#' @examples
#' # Create plot for benchmarks
#' library(ggplot2)
#' bench <- bigobenchmark(1:n, for(i in 1:n) for(i in 1:n) 1:n, args=seq(from=1, to=100, length.out = 50))
#' autoplot(bench)
#'
#' @author Andrew Prokhorenkov
autoplot.bigobenchmark <- function(object) {
  ggplot(data = object$benchmarks, aes(x=arg, y=mean, colour=expr)) +
    geom_line() +
    geom_pointrange(aes(ymin=min, ymax=max))
}
