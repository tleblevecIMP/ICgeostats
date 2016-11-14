
cov_2d_lat<-function(data,size,ncell){
  cov<-numeric(size)
  for (l in 1:size){
    npair = nrow(data)-l
    for ( pair in 1:npair){
      for ( y in 1:ncol(data)){
        cov[l] = cov[l]+ data[pair,y]*data[pair+l,y]
      }
    }
    cov[l]=cov[l]/(npair*ncol(data))
  }
  #cov = (cov - mean(data)^2 )/ var(data) # we are measuring a correlation
  dist = (1:size)/ncell
  #plot(dist,cov,type = "l")
  return(cov)
}

tr_2d_lat<-function(data,size,ncell){
  cov<-numeric(size)
  for (l in 1:size){
    npair = nrow(data)-l
    for ( pair in 1:npair){
      for ( y in 1:ncol(data)){
        cov[l] = cov[l]+ data[pair,y]*data[pair+l,y]
      }
    }
    cov[l]=cov[l]/(npair*ncol(data))
  }
  #cov = (cov - mean(data)^2 )/ var(data) # we are measuring a correlation
  cov= cov / mean(data)
  dist = (1:size)/ncell
  #plot(dist,cov,type = "l")
  return(cov)
}

tr_2d_vert<-function(data,size,ncell){
  cov<-numeric(size)
  for (l in 1:size){
    npair = ncol(data)-l
    for ( pair in 1:npair){
      for ( x in 1:nrow(data)){
        cov[l] = cov[l]+ data[x,pair]*data[x,pair+l]
      }
    }
    cov[l]=cov[l]/(npair*nrow(data))
  }
  #cov = (cov - mean(data)^2 )/ var(data) # we are measuring a correlation
  cov = cov/mean(data)
  dist = (1:size)/ncell
  #plot(dist,cov,type = "l")
  return(cov)
}
