#' Apply jonckheere terpstra test.
#'
#' @param data data.frame with cols value and s_cat
#' @param alpha numeric: level of significance
#' @param order vector of numeric: assumed order of the samples
#'
#' @return True if H0 is believed to be true, Flase if not
#' @export
#'
#' @examples jonckheere_terpstra(data.frame(value = c(1,1,1,2,2), sample = c(1,1,1,2,2)), 0.06, c(1,2))
#'
jonckheere_terpstra <- function(data, alpha, order) {
  ## validation check ##########################################################
  if (!is.data.frame(data)) {
    stop("data not dataframe")
  }
  if (!is.numeric(alpha)) {
    stop("alpha not numeric")
  }
  if(!is.numeric(order)) {
    stop("order is not numeric")
  }
  if(!is.vector(order)) {
    stop("order not a vector")
  }

  ## normal routine ############################################################
  test_stat <- 0
  n <- c()
  order_fac <- as.factor(order)
  for (i in 1:length(order)) {
    group_i <- data %>%
      dplyr::filter(s_cat == order_fac[i])

    n <- append(n, length(group_i))

    for (j in as.factor(order[(i+1):length(order)])) {
      group_j <- data %>%
        dplyr::filter(s_cat == j)
      for (xij in group_i$value) {
        test_stat <- test_stat + sum(group_j$value < xij)
      }
    }
  }
  N <- length(data$value)

  mean_test <- (N^2 - sum(n^2))/4
  var_test <- ((N^2*(2*N + 3) - sum(2*n^3 + 3*n^2)))/72

  crit_val <- qnorm(alpha,
                    lower.tail = F)

  test_val <- (test_stat - mean_test)/sqrt(var_test)

  return(test_val <= crit_val)
}
