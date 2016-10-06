# monte carlo simulation of drilling n wells
# we use a uniform distribution and threshold it according to the Ps

drilling_well_MC<-function(n_wells,Ps,n_trials){
  monte_carlo <-integer(length=n_trials)
  for ( i in 1:n_trials){
    realization=runif(n_wells,min=0,max=1)
    realization[realization>Ps]<-0
    realization[realization<Ps & realization != 0]<-1
    monte_carlo[i]=sum(realization)
  }
  hist(monte_carlo)
}
