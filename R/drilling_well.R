# success probability when drilling wells

drilling_well<-function(n,Ps){
  proba =1
  for ( i in 1:n){
    proba = proba*(1-Ps)
  }
  return(1-proba)
}
