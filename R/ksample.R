
#' simulating k samples based on given densities
#' @description ksamples generates independent samples based on given densities.
#'
#' @param randfuncs a vector of functions with length k: the densities for the samples
#' @param size a vector of integers with length k: the list of sample-sizes
#'
#' @return a dataframe with columns "value" (realized observation) and "sample" (sample (1 to k) to which the observation belongs to)
#' @export
#'
#' @examples generate_ksamples(2, c(rnorm, rnorm))
#' @export
generate_ksamples <- function(randfuncs, size="random") {
  ## function config ###########################################################
  # sample sizes
  sample_s_lower_border <- 6
  sample_s_upper_border <- 120
  random_sizes <- function(x) {
    return(as.integer(
        runif(
            x,
            sample_s_lower_border,
            sample_s_upper_border
        )
      )
    )
  }
  ## validate the input #######################################################
  k_int <- length(randfuncs)
  # check size
  if(size[1] != "random") {
    if(!is.numeric(size)) {
      stop("size must be either random or a vector of numerics")
    }
    else if (length(size)!= k_int) {
      stop("size_vector must have legth of k ")
    }
  }
  # check randfunc
  if(length(randfuncs) != k_int) {
    stop("randfunc has not length of k")
  }
  for (i in randfuncs) {
    if(!is.function(i)) {
      stop("randfuncs holds non functions")
    }
  }

  ## normal routine ############################################################
  random_sample_size <- size == "random"

  result <- data.frame(value=NULL, sample=NULL)
  sample_cat <- factor(1:k_int)
  i <- 1
  if (random_sample_size[1]) {
    size <- random_sizes(k_int)
  }
  tryCatch(
    expr = {
      for (rand in randfuncs) {
        values <- rand(size[i])
        samples <- rep(sample_cat[i], size[i])
        result <- rbind(result, data.frame(value=values, s_cat=samples))
        i = i+1
      }

      return(result)
    },
    error = function(e) {
      message("error:\n", e)
    },
    warning = function(w) {
      message("warning:\n", w)
    }
  )

}
