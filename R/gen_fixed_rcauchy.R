#' fix location and scale params of rcauchy function
#'
#' @param location location of rcauchy function
#' @param scale scale of rcauchy function
#'
#' @return fixed rcauchy function
#' @export
#'
#' @examples
gen_fixed_rcauchy <- function(location, scale) {
  force(location)
  force(scale)
  return(function(n) {
    return(rcauchy(n, location, scale))
  })
}
