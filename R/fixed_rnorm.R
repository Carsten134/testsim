
#' fixing params on rnorm function
#'
#' @param mean mean of rnorm
#' @param var variance of rnorn
#'
#' @return function(n) where n is the sample size of generated sample
#' @export
#'
#' @examples fixed_rnorm(10,2)
fixed_rnorm <- function(mean, var) {
  force(mean)
  force(var)
  return(function(x) {
    rnorm(x,mean,var)
  })
}
