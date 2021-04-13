#' @ title A maximum likelihood function
#'
#' @param lfun The distribution function to sample from
#' @param x A vector of measurements
#' @param param A range of values to check for parameter
#' @param ... Additional plot details
#'
#' @return A list of data about a plot, and a plot
#' @export
#'
#' @examples
#' \dontrun mymaxlik(function(x,param) log(dbinom(x,prob=param,size=20)),
#' \dontrun     c(1,4,2),seq(0,1,length=1000))
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function's eval
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, so 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/
             (param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
