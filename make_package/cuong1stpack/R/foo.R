#' This is the first function.
#' 
#' \code{foo} return some info
#' 
#' @param args = NA you need no argument
#' @return several strings and the number of cores in
#'          your computer.
#' @examples
#'    foo (args = NA)

foo <- function (args = NA) {
  # This function just print stuff blah
  print("Haha it works!")
  print("Exit now. Cuong is the best.")
  print("exit and then come back to haunt you! hahaha")
  print(parallel::detectCores(logical = TRUE))
} 


bar <- function (a, b) {
  return (a + b)
}