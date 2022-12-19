#' generate vector of fixed rcauchy functions
#'
#' @param params data.frame with cols scale and location
#'
#' @return vector of fixed rcauchy functions
#' @export
#'
#' @examples
gen_rcauchy_vector <- function(params) {
  force(params)
  result <- c()
  for(i in 1:length(params$location)) {
    location <- params$location[i]
    scale <- params$location[i]
    result <- append(result, gen_fixed_rcauchy(location, scale))
  }
  return(result)
}
