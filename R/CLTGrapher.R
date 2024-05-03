#' Central Limit Theorem Plotter
#'
#' @param n number of samples used
#' @param iter number of times we run the function for the samples
#'
#' @return Histogram of r uniform distribution showing concepts of the central limit theorem
#' @export
#'
#' @examples
#' myclt(n=10,iter=10000)
myclt=function(n,iter)
{
  y=runif(n*iter,0,5)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
}
