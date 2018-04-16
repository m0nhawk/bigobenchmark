#' Autoplot for bigobenchmark object
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
#'
#' @author Andrew Prokhorenkov
autoplot <- function(object) {
  ggplot(data = object, aes(x=arg, y=mean, colour=expr)) +
    geom_line() +
    geom_pointrange(aes(ymin=min, ymax=max))
}
