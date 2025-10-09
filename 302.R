n1=4
n2=10
n3=24
n4=12
n5=6
n6=3
n=n1+n2+n3+n4+n5+n6
n
chisquare=function(a){
  p1=(1-exp(-1/a));p2=exp(-1/a)-exp(-2/a);p3=exp(-2/a)-exp(-3/a);p4=exp(-3/a)-exp(-4/a);p5=exp(-4/a)-exp(-5/a);p6=exp(-5/a)
  term1=(n1-n*p1)^2/(n*p1);term2=(n2-n*p2)^2/(n*p2);
  term3=(n3-n*p3)^2/(n*p3);term4=(n4-n*p4)^2/(n*p4);
  term5=(n5-n*p5)^2/(n*p5);term6=(n6-n*p6)^2/(n*p6)
  term=term1+term2+term3+term4+term5+term6
}
est=optimize(chisquare,c(0,10))
est$min
est$objective
curve(chisquare,c(0,10),col='blue',xlim = c(0,10),lwd=2.,xlab = "theta",ylim=c(20,70),ylab="chisquare theta")
abline(h=est$objective,v=est$min,col=c(2,3),lwd=2,lty=c(1,4))

222
n1=4
n2=10
n3=24
n4=12
n5=6
n6=3
n=n1+n2+n3+n4+n5+n6
n
chisquare=function(a){
  p1=(1-exp(-a));p2=exp(-a)-exp(-2*a);p3=exp(-2*a)-exp(-3*a);p4=exp(-3*a)-exp(-4*a);p5=exp(-4*a)-exp(-5*a);p6=exp(-5*a)
  term1=(n1-n*p1)^2/(n*p1);term2=(n2-n*p2)^2/(n*p2);
  term3=(n3-n*p3)^2/(n*p3);term4=(n4-n*p4)^2/(n*p4);
  term5=(n5-n*p5)^2/(n*p5);term6=(n6-n*p6)^2/(n*p6)
  term=term1+term2+term3+term4+term5+term6
}
est=optimize(chisquare,c(0,10))
est$min
est$objective
curve(chisquare,c(0,10),col='blue',xlim = c(0,10),lwd=2.,xlab = "theta",ylim=c(20,70),ylab="chisquare theta")
abline(h=est$objective,v=est$min,col=c(2,3),lwd=2,lty=c(1,4))

333
library(stats4)
x=c(105,140,20,113,121,10,44,150,60,30,30,11)
n=length(x)
logL=function(a,b){
  term1 =-n*log(b/a)
  term2=-(b-1)*sum(log(x/a))
  term3=sum((x/a)^b)
  term1+term2+term3
}
fit=mle(minuslogl = logL,start = list(a=quantile(x,0.632),b=1.5))
coefs=coef(fit)
var=vcov(fit)
coefs
var
diag(var)
quantile(x,0.632)
coefs[1]-1.96*sqrt(diag(var)[1])
coefs[1]+1.96*sqrt(diag(var)[1])
coefs[2]-1.96*sqrt(diag(var)[2])
coefs[2]+1.96*sqrt(diag(var)[2])
qweibull(0.7,scale = coefs[1],shape = coefs[2])

44
library(stats4)
x=c(105,140,20,113,121,10,44,150,60,30,30,11)
n=length(x)
logL=function(a,b){
  term1 = -n * log(1/gamma(a))
  term2 = -n * a * log(b)            
  term3 = -(a - 1) * sum(log(x))   
  term4 = sum(x)*b
  term1+term2+term3+term4
}
fit=mle(minuslogl = logL,start = list(a=quantile(x,0.632),b=1.5))
coefs=coef(fit)
var=vcov(fit)
coefs
var
diag(var)
quantile(x,0.632)
coefs[1]-1.96*sqrt(diag(var)[1])
coefs[1]+1.96*sqrt(diag(var)[1])
coefs[2]-1.96*sqrt(diag(var)[2])
coefs[2]+1.96*sqrt(diag(var)[2])
qgamma(0.75,rate = coefs[2],shape = coefs[1])

55
a1 = 0.025
a2 = 0.025
x = c(86, 146, 251, 623, 98, 175, 176, 76, 264, 15, 157, 220, 42, 321,
      180, 198, 38, 20, 61, 121, 282, 224, 189, 180, 325)

s = sum(x)
n = length(x)
z1=qchisq(a1,2*n)
z2=qchisq(1-a2,2*n)
L1=z1/(2*s)
L2=z2/(2*s)
PvTM=c(L1,L2)
PvTM
L_1=(qchisq(a1,2*(n+1)))/(2*(s+1))
L_2=(qchisq(1-a2,2*(n+1)))/(2*(s+1))
cbind(L_1,L_2)

666
x = c(2.1, 5.2, 2.3, 1.4, 2.2, 2.3, 1.6)
k = 2

theta1 = median(x)
d = median(abs(x - theta1)) / 0.6745
h = (x - theta1)/d
psi <- ifelse(h < -k, -k,
              ifelse(h < k, x, k)
)

psi.prime = ifelse(h < -k, 0, ifelse(h < k, 1, 0))

theta2 = theta1 + d*sum(psi) / sum(psi.prime)


theta1
theta2
h2 = (x - theta2)/d
psi <- ifelse(h2 < -k, -k,
              ifelse(h2 < k, h2, k)
)

psi.prime = ifelse(h2 < -k, 0, ifelse(h2 > k, 0, 1))

theta3 = theta2 + d*sum(psi) / sum(psi.prime)
theta3

var = d^2 * sum(psi^2) / (sum(psi.prime))^2
var

## Variance of xbar
var(x)/n