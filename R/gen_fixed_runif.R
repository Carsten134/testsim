#' Fix uniform destribution parameters
#'
#' @param min minimum value
#' @param max maxmimum value
#'
#' @return function with fixed params
#' @export
#'
#' @examples gen_fixed_runif(0,2)
gen_fixed_runif <- function(min, max) {
  force(min)
  force(max)
  return(function(x) {
    return(runif(x, min, max))
  })
}
