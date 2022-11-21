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

simulate <- function (x, params, maxstep = 1500) {
  output <- array(dim=c(maxstep+1,4))
  colnames(output) <- names(x)
  output[1,] <- x
  k <- 1
  while ((k <= maxstep) && (x["Y"] > 0)) {
    k <- k+1
    output[k,] <- x <- sim(x,params)
  }
  as.data.frame(output[1:k,])
}
nsims <- 10
xstart <- c(time=0,X=760,Y=3,Z=0)
params <- c(beta=0.450626519034621,gamma=0.00221583886792478)

require(plyr)
simdat <- rdply(
  nsims,
  simulate(xstart,params)
)
d_ply(simdat,".n",function(x)lines(Y~time,data=x,col=.n))
