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
    median <- oder_stat[(N+1)/2]
  }

  # compute m_1i

}
