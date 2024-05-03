#' ntickets
#'
#' @param N Number of seats on the flight.
#' @param gamma probability of a plane being overbooked.
#' @param p probability of someone who bought a ticket showing up.
#'
#' @return A named list including n (number of tickets to be sold) derived discretely and normaly, N, p, and gamma. Also makes a graph for the discretely and normally distributed values.
#' @export
#'
#' @examples
#' ntickets(N=400, gamma=0.02, p=0.95)
ntickets = function(N, gamma, p)
{
  # Approximate Discrete Distrobution Method
  nRange = N:(N+20) # Adding 20 more seats than N as specified in project description
  ndFun <- function(n) # Function of n for discrete random variables, used to populate ndVector
  {
    1 - gamma - pbinom(N, n, p)
  }
  ndVector = sapply(nRange, ndFun) # Applying the ndFun function to every value in the nRange vector and storing that in ndVector
  nd = nRange[which.min(abs(ndVector))] # Finding the minimum (absolute) value in ndVector and using that as an index in nRange to find our proper nd

  # Approximate Normal Distrobution Method
  ncFun <- function(n) # Function of n for continuous random variables, used to populate ncVector
  {
    abs(1 - gamma - pnorm(N, mean = n*p, sd = sqrt(n*p*(1-p))))
  }

  ncVector = sapply(nRange, ncFun) # Needed for graph, same idea as discrete counterpart
  optimizedNC = optimize(f = ncFun, interval = c(N, N+20)) # Finding the minimum value of the ncFun function and storing it in optimizedNC
  nc = optimizedNC$minimum # Pulling the minimum out of optimizedNC and storing it in nc

  # Creating and printing named list specified in project description
  namedList = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(namedList)


  # Plots for Discrete and Normal approximations

  #Discrete Approximation
  plot(nRange,ndVector, type = "b", col = "black", pch = 20, # type b for dots and lines, pch = 20 for solid dots as seen in project example
       xlab = "n", ylab = "Objective",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",nd,") gamma = ",gamma,", N = ",N,", Discrete"))
  abline(v = nd, col = "red")  # Creating a red vertical line at nd
  abline(h = ndVector[which(nRange == nd)], col = "red") # Creating a red horizontal line at the index of nd in the ndVector (y-axis)

  # Normal Approximation
  plot(nRange, ncVector, type = "l", col = "black", xlab = "n", ylab = "Objective", # Type l for line only
       main = paste0("Objective Vs n to find optimal tickets sold\n(",nc,") gamma = ",gamma,", N = ",N,", Continuous"))
  abline(v = nc, col = "blue")  # Creating a red vertical line at nc
  abline(h = ncFun(nc), col = "blue") # Creating a red horizontal line at the index of nc in the ncVector (y-axis)

}
