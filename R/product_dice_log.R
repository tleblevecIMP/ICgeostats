# log of the dice product

product_dice_log<-function(n_dice,n_trials){
  # Preparation
  dice<-c(1,2,3,4,5,6)
  monte_carlo <-integer(length=n_trials)

  # Simulation
  for ( i in 1:n_trials){
    realization = sample(x=dice,size=n_dice,replace=TRUE)
    monte_carlo[i] = prod(realization)
  }
  hist(log(monte_carlo),freq=FALSE)
}
