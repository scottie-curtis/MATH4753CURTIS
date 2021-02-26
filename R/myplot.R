#' @title An introductory plot function
#'
#' @param x A quantitative vector
#'
#' @return A plot of a downward parameter with the given domain
#' @export
#'
#' @examples
#' \dontrun{d <- 1:40; myplot(x = d)}
myplot=function(x){
  y <- 0.86089580 +1.46959217*x  -0.02745726*x^2
  plot(y~x,col="forestgreen",lwd=3,type="l",main="Downward Parabola")
}
