#' Apply Median test
#'
#' @param data dataframe with cols "value" and "sample"
#' @param alpha level of signficance
#'
#' @return False if H_0 is believed to be false and True else
#' @export
#'
#' @examples mood_test(data, 0.05)
mood_test <- function(data, alpha) {
  ## checking validity #########################################################

  ## normal routine ############################################################

  # estimate the median
  values <- data$value
  N <- length(values)
  order_stat <- sort(values)

  if (N %% 2 == 0) {
    median <- 0.5*(order_stat[N/2] + order_stat[N/2+1])
  } else {
    median <- order_stat[(N+1)/2]
  }

  # compute m_1i
  n <- data %>%
    dplyr::group_by(s_cat) %>%
    dplyr::count()

  m_i <- c()
  m <- values <= median
  curr <- 1
  a_1 <- 0
  k <- length(n$n)
  for (n_i in n$n) {
    curr_val <-  sum(m[curr:(curr + n_i - 1)])
    m_i <- append(m_i, curr_val)
    curr <- curr + n_i
    a_1 <- a_1 + curr_val
  }

  sum <- 0
  for (i in 1:k) {
    n_i <- n$n[i]
    sum <- sum + (m_i[i] - (n_i*a_1)/N)^2/n_i
  }

  A <- (N^2/(a_1*(N-a_1)))*sum

  # computing critvalues
  approx <- T
  for (i in n$n) {
    if (i <= 5) {
      approx <- F
    }
  }
  if (N <= 25) {
    approx <- F
  }

  if (approx) {
    crit_val <- qchisq(alpha, k-1,
                       lower.tail = F)
  }

  # check for H_0
  return(A <= crit_val)
}
