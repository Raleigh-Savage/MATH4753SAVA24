#' Sample Distribution Plot
#'
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#'
#' @param n Sample size for each iteration
#' @param iter Number of iterations
#' @param time the pause between iterations
#'
#' @return Nothing, intended purpose is for visualization
#' @export
#'
#' @examples
#' mySample30 = SampleDistPlot(n=1000, iter=30, time = 1)
SampleDistPlot = function(n, iter=10,time=0.5)
{
  for( i in 1:iter)
  {
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2))

    #release the table
    Sys.sleep(time)
  }
}
