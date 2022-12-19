#' generate fixed runif functions
#'
#' @param params data.frame with cols min, max
#'
#' @return vector of fixed runif functions
#' @export
#'
#' @examples gen_runif_vector(data.frame(min = c(1,2,3), max = c(2,2,2)))
gen_runif_vector <- function(params) {
  force(params)
  result <- c()
  for (i in 1:length(params$max)) {
    min <- params$min[i]
    max <- params$max[i]
    result <- append(result, gen_fixed_runif(min, max))
  }
  return(result)
}
