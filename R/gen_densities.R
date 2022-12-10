
#' generating densities
#' @description generates densities for sample generation. Note that this is a function
#' for general szenarios. I highly encourage experimenting with less general densities.
#' @param k number of densities to be created
#' @param kind can take on values ["norm", "unif"]:kind of densities to be generated
#' @param offset offset of the mean
#' @param application can take on values ["to_one", "to_all"] to which the offset applies
#' @param var number or vector of length k: variance of the destributions
#'
#' @return list of functions
#' @export
#'
#' @examples
gen_densities <- function(k, kind="norm", offset=0, application="to_all", var=1) {
  ## function config ###########################################################
  valid_kinds <- c("norm", "unif", "")
  valid_applications <- c("to_all", "to_one")

  ## check validity ############################################################
  if (!is.numeric(k)) {
    stop("k not numberc")
  }
  if (!is.integer(k)) {
    k_int <- as.integer(k)
  }
  if (!(kind %in%  valid_kinds)) {
    stop("kind not a valid value, check documentation for valid values")
  }

  if(!is.numeric(offset)) {
    stop("offset not a numeric value")
  }
  if (!(application %in% valid_applications)) {
    stop("application not a valid value, check documentation for valid values")
  }
  if (!is.numeric(var)) {
    if(is.vector(var)) {
      if(!(length(var) == k)) {
        stop("var has no required length")
      }
    }
    stop("var not of type vector or numeric")
  }
  ## normal routine ############################################################

  if (!exists("k_int")) {
    k_int <- k
  }
  means <- rep(0, k_int)
  if (kind == "to_one") {
    sample_off_ind <- as.integer(runif(1)*k_int)
    means[sample_off_ind] <- offset
  } else if (kind == "to_all") {
    means <- rep(offset, k_int)
  }


  if (length(var) == 1) {
    var_values <- rep(var, k_int)
  }

  result = c()
  density_funcs <- switch (kind,
    "norm" = rnorm,
    "unif" = runif
  )
  for (i in 1:k_int) {
    # dont know how to deal with this issue
    # means[i] just gives a pointer to the value
    # but after execution the value does not exist anymore
    # we need to copy a numeric value
      mean <- means[i]
      var <- var_values[i]
      if (kind == "norm") {
        result <- append(
          result,
          function(x) {
            rnorm(x, mean, var)
          })
      } else if (kind == "unif") {
        result <- append(
          result,
          function(x) {
            runif(x, mean, var)
          })
      }

  }
  return(result)
}
