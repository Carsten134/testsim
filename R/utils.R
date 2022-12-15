#' Benchmarking functions
#'
#' @param eval function to be benchmarked
#'
#' @return miliseconds taken to run the function
#' @export
#'
#' @examples timer(eval)
timer <- function(eval) {
  start_time <- Sys.time()
  eval()
  end_time <- Sys.time()
  return(as.numeric(end_time - start_time))
}
