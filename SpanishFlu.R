
# Change to whichever working directory needed
setwd("C:/Users/joely/Desktop/InfectiousDiseases/ProjectWriteup");

# Load libraries
library(deSolve);
library(ggplot2);
library(reshape2);
# Graphics
colours <- c('green','red', 'blue', 'black');
# Data-set
df <- data.frame(read.csv('SpanishFluDataset.csv', header=TRUE));
# Population data
N <- 3.4*10^7;
birth_rate <- 0;
death_rate <- 0;
# Initial conditions
initial_conditions <- c(S=N-5, I=5, R=0);

parameter_start <- c(beta=1.5, gamma=0.222222222, sigma=0, sd=15);

# SIRS model
model <- function(params, initial_conditions, times){
  equations <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      # Define the differential equations
      dS <- -beta * S * I / N + sigma*R + (birth_rate - death_rate)*S;
      dI <- beta * S * I / N - (gamma+death_rate)*I;
      dR <- gamma * I - (sigma+death_rate)*R;
      
      # Return the results as a list
      list(c(dS,dI, dR));
    })
  }
  # Always have to pass t=0 state to ode solver as it gets it's IC from t=0.
  out<-ode(y = initial_conditions, times = c(0,times), func = equations, parms = params);
  
  return(as.data.frame(out)[-1,]);
  
}

# Normal likelihood
likelihood <- function(parameters){
  model_data <- model(parameters, times=df$day, initial_conditions=initial_conditions)$I;
  -sum(dnorm(x=df$cases, sd=as.numeric(parameters['sd']),mean=model_data, log=TRUE));
}
estimated_parameters <- optim(parameter_start, likelihood, lower=c(0,0,0,1), upper=c(10,0.25,1,10), method="L-BFGS-B");

estimated_parameters;
graph_times <- seq(0, tail(df$day,n=1), 0.5);
estimated_model<-model(estimated_parameters$par, initial_conditions, graph_times);
estimated_model<-melt(estimated_model, id.vars="time");
ggplot(NULL, aes(v, p)) + 
  geom_line(data=estimated_model, aes(time,value, col=variable), linewidth=1) +
  geom_point(data=df, aes(day, cases))+coord_cartesian(ylim = c(0, 25000))+scale_color_manual(values=colours);

