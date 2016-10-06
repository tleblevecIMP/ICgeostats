# computes the vario from a vector at one lag

vario_origin<-function(seq){
  vario = 0
  pairs = 0
  for ( i in 2:length(seq)){
    if(!is.na(seq[i]) && !is.na(seq[i-1])){
      vario = vario + (seq[i]-seq[i-1])^2
      pairs = pairs +1
    }

  }
  vario =vario / (2 *pairs)

  return(vario)
}
