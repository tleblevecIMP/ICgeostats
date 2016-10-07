# Monte Carlo simulation of the STOOIP
# need the package mc2d for the triangular distribution
library(mc2d)

STOOIP<-function(n_trials){
  # log normal distribution for the GRV -> high uncertainty, asymmetric on the reservoir top level
  # transformation from non log to log moments
  m = 10
  v =25
  meanlog = log((m^2)/(sqrt(v+m^2)))
  sdlog = sqrt(log(1+v/(m^2)))
  GRV=rlnorm(n_trials,meanlog=meanlog,sdlog=sdlog)
  # Triangular distribution for the N/G
  NG=rtriang(n_trials,min=0.4,mode=0.7,max=0.8) # to define with the geologist
  # normal distribution for the porosity
  Phi=rnorm(n_trials,mean=0.25,sd=0.02) # central limit theorem, arithmetic mean
  # log normal distribution for the So
  So=rtriang(n_trials, min = 0.3, mode=0.75, max=0.85)

  STOOIP = GRV*NG*Phi*So*0.75
  hist(STOOIP,breaks=100,freq=FALSE,xlim=c(0,10))
  return(STOOIP)
  }
