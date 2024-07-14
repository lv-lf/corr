#' @title Correlation
#'
#' @description `corr1` compute the correlation of x and y if these are vectors.
#'
#' @details The inputs must be numeric, and the vectors x and y are equal in
#' length.
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return a number
#' @export
#' @examples x <- 1:5
#' y <- c(3, 5, 8, 9, 13)
#' corr1(x, y)
corr1 <- function(x, y) {
  xy <- sum((x - mean(x)) * (y - mean(y)))
  xx <- sum((x - mean(x))^2)
  yy <- sum((y - mean(y))^2)
  r1 <- xy / sqrt(xx * yy)
  return(r1)
}

#' @title Correlation
#'
#' @description `corr2` compute the correlation of x and y if these are vectors.
#'
#' @details The inputs must be numeric, and the vectors x and y are equal in
#' length.
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return a number
#' @export
#' @examples x <- 1:5
#' y <- c(3, 5, 8, 9, 13)
#' corr2(x, y)
corr2 <- function(x, y) {
  r2 <- stats::cov(x, y) / sqrt(stats::var(x) * stats::var(y))
  return(r2)
}
