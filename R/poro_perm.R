# compute correlation between porosity and permeability

poro_perm<-function(){
  poro<-c(8,21,12,12,10,16,23,5,19,15,11,14,10,6,17,9,16,15,15,20)
  perm<-c(33,250,29,32,15,120,412,8,230,83,54,63,18,13,129,37,350,36,95,512)
  plot(poro,perm)
  plot(poro,log(perm))
  cov(poro,perm)/sqrt(var(perm)*var(poro))
  rho=cov(poro,log(perm))/sqrt(var(log(perm))*var(poro))

  # permeability as Y
  b1=rho*sd(log(perm))/sd(poro)
  a1= mean(log(perm))-b1*mean(poro)
  curve(b1*x+a1,add=TRUE)

  # permeability as X
  b2 = rho*sd(poro)/sd(log(perm))
  a2= mean(poro)-b2*mean(log(perm))
  curve((x-a2)/b2,add=TRUE)
}
