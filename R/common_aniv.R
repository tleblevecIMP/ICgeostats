# computes the probability of having at least two people with the same aniversary

common_aniv<-function(n_people){
  proba=1
  for ( i in 1:n_people){
    proba = proba * (365-i)/365
  }
  return( 1-proba)
}
