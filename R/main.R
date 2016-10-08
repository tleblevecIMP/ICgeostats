#M1
# dice
sum_dice(1,10000)
sum_dice(2,10000)
sum_dice(3,10000)
sum_dice(6,10000)
sum_dice(100,10000)
product_dice(1,10000)
product_dice(2,10000)
h<-product_dice(100,10000)
hist(h,freq=FALSE)
hist(log(h),freq = FALSE) # if we take the log we have a normal distribution, which means it is lognormal

# variance
zi = c(13,17,15,23,27,26,18,27,20,24)
zicentered = zi-mean(zi)
squaredcentered = zicentered^2
squared=zi^2
mean=sum(zi)/length(zi)
var=sum(squaredcentered)/length(zi)
var=sum(squared)/length(zi) - mean(zi)^2
mean(zi)
var(zi) # difference because it is the sample variance (-1)

# STOOIP
st<-STOOIP(1000)
quantile(st,0.1) # p90: worst case
quantile(st,0.5) # p50: probable case
quantile(st,0.9) # p10: best case

# poro-perm covariance
poro<-c(0.08,0.21,0.12,0.12,0.10,0.16,0.23,0.05,0.19,0.15,0.11,0.14,0.10,0.06,0.17,0.09,0.16,0.15,0.15,0.20)
perm<-c(33,250,29,32,15,120,412,8,230,83,54,63,18,13,129,37,350,36,95,512)
plot(poro,perm)
cov(poro,perm)/sqrt(var(perm)*var(poro))
plot(poro,log(perm),xlab="porosity")
rho=cov(poro,log(perm))/sqrt(var(log(perm))*var(poro))
print(rho)
# permeability as Y
b1=rho*sd(log(perm))/sd(poro)
a1= mean(log(perm))-b1*mean(poro)
curve(b1*x+a1,add=TRUE)
# permeability as X
b2 = rho*sd(poro)/sd(log(perm))
a2= mean(poro)-b2*mean(log(perm))
curve((x-a2)/b2,add=TRUE)

#M2
# vario at first lag
vario_origin(c(0,-1,-2,-1,NA,1,2,1,2,1,2))

# gaussian field simulation, vario sensitivity
# this function requires RGeostats: http://rgeostats.free.fr
windows()
simulate_from_vario()

#M3
#common anniversary
people=1:50
proba = numeric(50)
for ( i in people){
  proba[i]=common_aniv(i)
}
plot(people,proba)

# drilling wells
# at least one chance of success
wells=1:11
proba=numeric(10)
for ( i in wells){
  proba[i]=drilling_well(i,0.2)
}
plot(wells,proba)
proba # we need 11 wells

# 5 wells
# Monte Carlo
drilling_well_MC(5,0.2,1000)

# Binomial
drilling_well(5,0.2) # 0.7 proba of success
dbinom(0,5,0.2) # 0 success
dbinom(1,5,0.2) # 1 success
dbinom(2,5,0.2) # 2 success
dbinom(3,5,0.2) # 3 success
dbinom(4,5,0.2) # 4 success
dbinom(5,5,0.2) # 5 success

#M4 facies simualation
# TGS PGS simulation
# this function requires RGeostats: http://rgeostats.free.fr
windows()
PGS_vario()
