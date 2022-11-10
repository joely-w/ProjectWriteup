sim <- function(x, params){
  X <- x[2]
  Y <- x[3]
  Z <- x[4]
  N <- X+Y+Z
  
  beta <- params['beta']
  gamma <- params['gamma']
  
  rates <- c(transmission=beta*X*Y/N,
             recovery=gamma*Y)
  
  transitions <- list(
    transmission=c(-1,1,0),
    recovery=c(0,-1,1)
  )
  rate_total <- sum(rates)
  if(rate_total==0)
    tau <- Inf
  else
    tau <- rexp(n=1,rate=rate_total)
  event <- sample.int(n=2,size=1,prob=rates/rate_total)
  
  x+c(tau,transitions[[event]])
}

simulate <- function (x, params, maxstep = 10000) {
  output <- array(dim=c(maxstep+1,4))
  colnames(output) <- names(x)
  output[1,] <- x
  k <- 1
  ## loop until either k > maxstep or
  ## there are no more infectives
  while ((k <= maxstep) && (x["Y"] > 0)) {
    k <- k+1
    output[k,] <- x <- sim(x,params)
  }
  as.data.frame(output[1:k,])
}
set.seed(56856583)
nsims <- 10
xstart <- c(time=0,X=392,Y=8,Z=0) #initial conditions
params <- c(mu=0.02,beta=60,gamma=365/13) #parameters

require(plyr)
simdat <- rdply(
  nsims,
  simulate(xstart,params)
)
