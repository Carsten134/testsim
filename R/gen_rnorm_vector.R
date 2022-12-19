
#' generate vector of rnorm functions
#'
#' @param params dataframe of parameters
#'
#' @return vector of rnorm functions of fixed parameters
#' @export
#'
#' @examples gen_rnorm_vector(data.frame(mean=c(1,1,2), var=c(1,1,1)))
gen_rnorm_vector <- function(params) {
  ## validation ################################################################

  ## normal routine ############################################################
  result <- c()
  for (i in 1:length(params$mean)) {
    mean <- params$mean[i]
    var <- params$var[i]
    result <- append(result, fixed_rnorm(mean, var))
  }

  return(result)
}
