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
#' bench <- bigobenchmark(cumsum(1:n), cumprod(1:n), args=seq(from=1000, to=10000, length.out = 50))
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
    gc(FALSE)

    n <- args[[i]]
    r[[i]] <- summary(do.call(microbenchmark::microbenchmark, c(exprs, list=list, times=times, check=check, control=control)))
    r[[i]]["arg"] <- n
  }
  return(structure(list(benchmarks=bind_rows(r)), class = "bigobenchmark"))
}

#' bigobenchmark exported operators and S3 methods
#'
#' The following functions are imported and then re-exported
#' from the bigobenchmark  package to avoid loading them.
#'
#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @export
NULL

#' Autoplot for bigobenchmark object
#'
#' @importFrom ggplot2 autoplot
#'
#' @param object
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' # Create plot for benchmarks
#' library(ggplot2)
#' bench <- bigobenchmark(cumsum(1:n), cumprod(1:n), args=seq(from=1000, to=10000, length.out = 50))
#' autoplot(bench)
#'
#' @author Andrew Prokhorenkov
autoplot.bigobenchmark <- function(object) {
  plt <- ggplot2::ggplot(data = object$benchmarks, ggplot2::aes(x = arg, y = mean, colour = expr))
  plt <- plt + ggplot2::geom_line()
  plt <- plt + ggplot2::geom_pointrange(ggplot2::aes(ymin = min, ymax = max))
  plt
}
