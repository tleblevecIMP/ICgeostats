# let s try the product of the dice

product_dice<-function(n_dice,n_trials){
  # Preparation
  dice<-c(1,2,3,4,5,6)
  monte_carlo <-integer(length=n_trials)

  # Simulation
  for ( i in 1:n_trials){
    realization = sample(x=dice,size=n_dice,replace=TRUE)
    monte_carlo[i] = prod(realization)
  }
  hist(monte_carlo,freq=FALSE)
  return(monte_carlo)
}
