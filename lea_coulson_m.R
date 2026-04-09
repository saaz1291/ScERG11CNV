#' Written by Google Gemini 
#' Estimate mutation events (m) using the Lea-Coulson Median Method
#' 
#' @param r0 The observed median number of mutants across replicates.
#' @return The estimated number of mutation events (m).
lea_coulson_m <- function(r0) {
  
  # The Lea-Coulson Equation (Equation 37): r0/m - log(m) = 1.24
  # We rearrange it to f(m) = 0 for the root-finder
  target_func <- function(m) {
    (r0 / m) - log(m) - 1.24
  }
  
  # We set an interval for the search. 
  # m must be > 0 and is practically always less than r0.
  result <- uniroot(target_func, interval = c(0.001, r0))
  
  return(result$root)
}