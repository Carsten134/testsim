#' Applying a monte carlo simulation
#'
#' @param eval stochastic process
#' @param n number of executions
#'
#' @return relative and absolute number of occurences
#' @export
#'
#' @examples monte_carlo(fun, 100)
monte_carlo <- function(eval, n) {
  ## validation ################################################################

  ## normal routine ############################################################
  results <- c()
  for(i in 1:n) {
    results <- append(results, eval())
  }
  data <- data.frame(value=results)

  absolute_r <- data %>%
    dplyr::count(value)

  relative_r <- sapply(reduce$n,
                  function(val) {
                    return(val/n)
                  })
  relative_r <- data.frame(relative=relative_r)

  result <- cbind(absolute_r, relative_r)

  return(result)
}
