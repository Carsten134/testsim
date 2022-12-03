
#' simulating k samples based on given densities
#' @description ksamples generates independent samples based on given densities.
#' @param k an integer: how many samples should be simulated
#' @param randfuncs a vector of functions with length k: the densities for the samples
#' @param size a vector of integers with length k: the list of sample-sizes
#'
#' @return a dataframe with columns "value" (realized observation) and "sample" (sample (1 to k) to which the observation belongs to)
#' @export
#'
#' @examples ksample(3, c(rnorm, rnorm, function(n) {return(rnorm(n, 0,2))})),  ksample(2, c(rnorm, rnorm))
#' @export
ksamples <- function(k, randfuncs, size="random") {
  ## function config ###########################################################
  # sample sizes
  sample_s_lower_border <- 0
  sample_s_upper_border <- 100
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
  # check k
  if (k < 1) {
    stop("k must be at least one")
  }
  if (!is.numeric(k)) {
    stop("k is not a number")
  }
  if(!is.integer(k)) {
    k_int <- as.integer(k)
  }
  # check size
  if(size[1] != "random") {
    if(!is.numeric(size)) {
      stop("size must be either random or a vector of numerics")
    }
    else if (length(size)!= k) {
      stop("size_vector must have legth of k ")
    }
  }
  # check randfunc
  if(length(randfuncs) != k) {
    stop("randfunc has not length of k")
  }
  for (i in randfuncs) {
    if(!is.function(i)) {
      stop("randfuncs holds non functions")
    }
  }

  ## special cases #############################################################
  if (k == 1) {
    tryCatch(
      expr = {
        if(size == "random") {
          return(randfunc(random_sizes(1)[1]))
        }
        else {
          return(randfunc(size[1]))
        }
      },
      error = function(e) {
        message("error:\n", e)
      },
      warning = function(w) {
        message("warning:\n", w)
      }
    )
  }

  ## normal routine ############################################################
  random_sample_size <- size == "random"

  result <- data.frame(value=NULL, sample=NULL)
  sample_cat <- factor(1:k)
  i <- 1
  if (random_sample_size) {
    size <- random_sizes(k)
  }
  tryCatch(
    expr = {
      for (rand in randfuncs) {
        values <- rand(size[i])
        samples <- rep(sample_cat[i])
        result <- rbind(result, data.frame(value=values, sample=samples))
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