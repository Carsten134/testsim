
#' applying kruksal wallis to a k-sample
#'
#' @param sample dataframe with cols "value" and "s_cat"
#' @param alpha value between 0 and 1, level of significance
#'
#' @return True if H_0 is believed to be true an Flase if not
#' @export
#'
#' @examples kruksal_wallis(data.frame(value = c(1,1,3,45,6), sample = factor(c(1,1,2,2,2)))
kruskal_wallis <- function(data_raw, alpha) {
  ## validating input ##########################################################
  if(!is.data.frame(data_raw)) {
    stop("sample input not numeric")
  }
  ## normal routine ############################################################
  # compute teststatistic
  rank_vals <- rank(data_raw$value, ties.method="random")
  data <- data.frame(value=data_raw$value,
                     s_cat=data_raw$s_cat,
                     rank=rank_vals)

  N <- length(data$value)

  rank_mean_df <- data %>%
    dplyr::group_by(s_cat) %>%
    dplyr::summarize(s_cat=s_cat,
                     rank_sum=sum(rank)) %>%
    dplyr::distinct(s_cat, rank_sum)

  sample_size <- data %>% dplyr::count(s_cat)

  processed_data <- cbind(sample_size, rank_mean_df)

  k <- length(processed_data$s_cat)
  sum <- 0
  for(i in 1:k) {
     sum <- sum + (processed_data$rank_sum[i]^2) /processed_data$n[i]
  }

  H <- (12)/(N*(N+1)) * sum - 3*(N+1)

  # compute critical values
  # is the exact destribution needed?
  exact_req <- F
  for (n_i in processed_data$n) {
    if(n_i <= 5) {
      exact_req <- T
    }
  }

  if (N <= 25) {
    exact_req <- T
  }

  if (!exact_req) {
    crit_val <- qchisq(alpha, k-1, lower.tail=F)
  } else {
    stop("exact destribution not yet implemented but needed")
  }

  # evalutation

  return(H < crit_val)
}
