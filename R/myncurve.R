#' @ title A function that plots a normal curve with a lower-tail region
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The upper bound of the probability
#'
#' @return A plot with a shaded region
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5,a=6)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma),
        xlim = c(mu-3*sigma, mu + 3*sigma)) # Plot curve
  xcurve<-seq(mu-3*sigma,a,length=1000) # Create vector of points
  ycurve<-dnorm(xcurve,mu,sigma) # Create corresponding y-values
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),
          col="skyblue") # Fill area
  prob<-round(pnorm(a,mu,sigma),4) # Calculate area
  text(x=(a+(mu-3*sigma))/2,
       y=dnorm((a+(mu-3*sigma))/4,mu,sigma),
       paste("Area = ", prob, sep="")) # Print area
}
