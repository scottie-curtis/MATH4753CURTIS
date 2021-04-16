#' @ title A CI for mean from a single sample
#'
#' @param x The reference sample
#' @param conf.level The desired confidence level
#'
#' @return A vector containing the lower and upper bounds of the CI
#' @export
#'
#' @examples
#' \dontrun set.seed(23);x = rnorm(30,mean=10,sd=12);myci(x)
myci = function(x,conf.level=0.95) {
  yhat = mean(x)
  s=sd(x)
  alpha = 1-conf.level
  n = length(x)
  t = qt(1-alpha/2,n-1)
  ci=c()
  ci[1]=yhat-t*s/sqrt(n)
  ci[2]=yhat+t*s/sqrt(n)
  return(ci)
}
