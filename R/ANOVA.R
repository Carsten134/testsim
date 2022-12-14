#' applying one factorial anova
#' @description one factorial anova tests for differences between samples
#'
#' @param data dataframe: with cols "value" and "sample"
#' @param alpha numeric: the level of significance
#'
#' @return True, if H_0 is believed to be true, Flase if not
#' @export
#'
#' @examples anova(data.frame(sample=factor(c(1,1,2)), value=c(1,2,3)), 0.05)
anova <- function(data, alpha) {
  ## validation ################################################################
  if (!is.numeric(alpha)) {
    stop("alpha not numeric")
  }
  if (!(alpha <= 1 && alpha > 0)) {
    stop("alpha not valid value: must be in (0,1]")
  }
  if (!is.data.frame(data)) {
    stop("data not dataframe")
  }
  ## normal routine ############################################################
  # computing the teststatistic
  mean <- mean(data$value)
  N <- length(data$value)

  mean_vals <- data %>%
    dplyr::group_by(s_cat) %>%
    dplyr::summarize(mean_vals=mean(value)) %>%
    dplyr::select(mean_vals)

  sample_size <- data %>% dplyr::count(s_cat)

  processed_data <- cbind(sample_size, mean_vals)

  k <- length(processed_data$s_cat)

  sum_outer <- 0
  sum_inner <- 0

  curr <- 1
  for(i in 1:k) {
    n_i <- processed_data$n[i]
    mu_i <- processed_data$mean_vals[i]
    sum_outer <- sum_outer + n_i*(mu_i - mean)^2
    vals <- data$value[curr:(curr + n_i - 1)]
    curr <- curr + n_i
    for(val in vals) {
      sum_inner <- sum_inner + (val - mu_i)^2
    }
  }

  F_stat <- (sum_outer/(k-1))/(sum_inner /(N-k))

  # compute crit vals
  crit_val <- qf(alpha,
                 df1 = (k-1),
                 df2 = (N-k),
                 lower.tail = F)

  return(F_stat < crit_val)
}
