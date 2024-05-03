#' myncurve
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @param mu Mean
#' @param sigma Standard Deviation
#' @param a Right endpoint for area
#'
#' @return A list containing the mean, standard deviation, and standard deviation.
#' @export
#'
#' @examples
#' myncurve(5, 10, 2)
myncurve = function(mu, sigma, a)
{

  x <- NULL

  # Creating the curve
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  list(mu = mu, sigma = sigma)

  # Setting x and Y value for shaded area
  xcurve=seq(mu - 10*sigma, a ,length=1000)
  ycurve=dnorm(xcurve,mean = mu,sd = sigma)

  # Create the shaded area
  polygon(c(mu - 10*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  # Get the probability by calculating the lower tail
  prob = pnorm(a, mean = mu, sd = sigma)
  prob=round(prob,4)

  # Return the three elements in a list
  return(list(mean = mu, sd = sigma, probability = prob))
}
