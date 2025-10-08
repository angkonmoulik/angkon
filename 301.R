install.packages("mvtnorm")
library(mvtnorm)
n=1000
n
set.seed(111)
m.vec=c(11,8,12,29,10)
m.vec
cov.mat=matrix(c(6,4,3,2,1,4,5,4,3,2,3,4,7,4,3,2,3,4,9,4,1,2,3,4,4),nrow = 5)
cov.mat
x=rmvnorm(n=n,mean=m.vec,sigma=cov.mat)--                                                                            
x
colnames(x)=c("x1","x2","x3","x4","x5")
x
head(x)
boxplot(x,col=c("green","red","blue","pink","yellow"))
par(mfrow=c(1,5))
hist(x[,1],col=2,breaks = 10,prob=T)
lines(density(x[,1]))
hist(x[,2],col=3,breaks = 10,prob=T)
lines(density(x[,2]))
hist(x[,3],col=4,breaks = 10,prob=T)
lines(density(x[,3]))
hist(x[,4],col=5,breaks = 10,prob=T)
lines(density(x[,4]))
hist(x[,5],col=6,breaks = 10,prob=T)
lines(density(x[,5]))

222222
rm(list=ls())
library(mvtnorm)
n=c(500,1500,6000)
mu=c(5,6)
mu
covmat=matrix(c(8,-3,-3,5),ncol=2)
covmat
set.seed(1351)
sample=list(rmvnorm(n[1],mu,covmat),rmvnorm(n[2],mu,covmat),rmvnorm(n[3],mu,covmat))
sample

library(stats4)
neg.11=function(mean1,mean2,s11,s12,s22)
{
  mean=c(mean1,mean2)
  s=matrix(c(s11,s12,s12,s22),2,2)
  log.11=sum(dmvnorm(X,mean,s,log=TRUE))
  return(-log.11)
}
X=sample[[1]]
xbar = colMeans(X)
S = var(X)

mle1 = mle(minuslogl=neg.11,start=list(mean1=xbar[1],mean2=xbar[2],
                                       s11=S[1,1],s12=S[1,2],s22=S[2,2]))
mle1
X=sample[[2]]
xbar = colMeans(X)
S = var(X)

mle2 = mle(minuslogl=neg.11,start=list(mean1=xbar[1],mean2=xbar[2],
                                       s11=S[1,1],s12=S[1,2],s22=S[2,2]))
mle2

X=sample[[3]]
xbar = colMeans(X)
S = var(X)

mle3 = mle(minuslogl=neg.11,start=list(mean1=xbar[1],mean2=xbar[2],
                                       s11=S[1,1],s12=S[1,2],s22=S[2,2]))
mle3
summary(mle1)
summary(mle2)
summary(mle3)

333333
y1=c(580,473,664,739,143,127,703,108,185,111,815,770,759,928,849)
y2=c(516,319,369,193,853,632,551,578,074,544,365,522,205,360,137)
y3=c(613,514,782,293,927,512,936,859,244,618,500,542,443,402,396)
y4=c(750,963,107,530,121,837,118,113,663,816,930,570,789,611,700)
y5=c(185,183,211,189,216,195,215,223,163,190,208,170,197,156,190)
y=data.frame(y1,y2,y3,y4,y5)
y
n=length(y1)
mu_o=c(108,500,600,700,180)
y1.bar=mean(y1)
y2.bar=mean(y2)
y3.bar=mean(y3)
y4.bar=mean(y4)
y5.bar=mean(y5)
y.bar=c(y1.bar,y2.bar,y3.bar,y4.bar,y5.bar)
s=cov(y)
s.inv=solve(s)
T2=n*t(y.bar-mu_o)%*%s.inv%*%(y.bar-mu_o)
n=nrow(y)
p=ncol(y)
F.dist=((n-p)/(p*(n-1)))*T2
F.dist
p.value=pf(F.dist,p,n-p,lower.tail = F)
p.value
out=list("T-squared"=T2,"p_value"=p.value)
out

44444
install.packages("Hotelling")
library(Hotelling)
cov1=matrix(c(0.46,1.18,4.49,1.18,7.40,-1.35,4.49,-1.35,4.24),ncol=3)
cov2=matrix(c(0.148,-0.679,0.209,-0.679,4.1,2.2,0.209,2.2,2.18),ncol=3)
x=list(mean=c(7,10,44),cov=cov1,n=30)
y=list(mean=c(5,4,2),cov=cov2,n=50)
fit=hotelling.test(x,y,var.equal=F)
fit

.555
install.packages("ellipse")
install.packages("jocre")
library(ellipse)
library(jocre)
library(mvtnorm)
set.seed(4186)
sigma=matrix(c(2,1.5,1.5,3),ncol=2)
mu=c(2,7)
p=cov2cor(sigma)
n=100
x=rmvnorm(n,mu,sigma)
plot(x,main = "Q5 bivwhehuehruhgr",xlab = "x",ylab = "y")
lines(ellipse(p,centre=c(2,7),col="blue"))
colnames(x)=c("A","B")
hotelling = cset(dat= x, method = "hotelling", alpha = 0.1)
summary(hotelling)


2023--44
# Q4
# Given data
X <- matrix(c(
  57,102,173,
  82,131,245,
  56,97,213,
  77,126,163,
  68,147,202,
  64,99,138,
  58,123,159,
  61,105,144,
  89,169,212,
  71,98,243,
  85,119,246,
  48,107,216
), ncol=3, byrow=TRUE)

Y <- matrix(c(
  51,93,197,
  20,118,213,
  17,88,131,
  17,106,174,
  14,93,176,
  15,80,168,
  61,130,125,
  52,77,187,
  36,84,141,
  64,90,147,
  74,121,183,
  54,110,203
), ncol=3, byrow=TRUE)

# Sample sizes
n1 <- nrow(X)
n2 <- nrow(Y)
p <- ncol(X)

# Sample means and covariances
xbar <- colMeans(X)
ybar <- colMeans(Y)
S1 <- cov(X)
S2 <- cov(Y)

# Pooled covariance matrix
Sp <- ((n1-1)*S1 + (n2-1)*S2) / (n1 + n2 - 2)

# Hotelling’s T² statistic
T2 <- (n1*n2/(n1+n2)) * t(xbar - ybar) %*% solve(Sp) %*% (xbar - ybar)

# F transformation
F_stat <- ((n1 + n2 - p - 1) / (p * (n1 + n2 - 2))) * T2
p_value <- 1 - pf(F_stat, df1=p, df2=(n1 + n2 - p - 1))

T2
F_stat
p_value

# Comment:
# If p_value < 0.05 → Reject H0 (mean vectors differ).
# Otherwise, fail to reject H0 (no significant difference between populations).
2023___4...


x1 <- c(57, 82, 56, 77, 68, 64, 58, 61, 89, 40, 71, 75, 86, 48)
x2 <- c(102, 131, 97, 126, 147, 99, 125, 110, 129, 106, 98, 138, 119, 107)
x3 <- c(173, 245, 213, 163, 220, 138, 158, 176, 212, 200, 243, 182, 246, 216)
X = cbind(x1, x2, x3)


y1 <- c(51, 20, 71, 17, 34, 15, 45, 61, 52, 23, 39, 66, 74, 54)
y2 <- c(93, 118, 88, 103, 96, 80, 98, 120, 77, 112, 84, 91, 121, 110)
y3 <- c(197, 213, 131, 174, 150, 178, 166, 125, 181, 177, 142, 191, 183, 203)
Y = cbind(y1, y2, y3)

# sample size of each group
n.x <- nrow(X)
n.y <- nrow(Y)

# mean of each group
mu.x <- colMeans(X)
mu.y <- colMeans(Y)

# covariance matrix of each group
cov.x <- cov(X)
cov.y <- cov(Y)

library(Hotelling)
x = list(mean = mu.x, cov = cov.x, n = n.x)
y = list(mean = mu.y, cov = cov.y, n = n.y)
fit = hotelling.test(x, y, var.equal = FALSE)
fit

2, c

library(mvtnorm)
n = c(2000, 5000, 8000)
mu = c(3, 7)

covmat = matrix(c(6, 5, 5, 7), 2)
set.seed(123)

sample = list(
  rmvnorm(n[1], mu, covmat),
  rmvnorm(n[2], mu, covmat),
  rmvnorm(n[3], mu, covmat)
)

par(mfrow = c(1, 3))

X = sample[[1]]
d.2 = c()

for(i in 1:n[1]){
  d.2[i] = t(X[i, ] - mu) %*% solve(covmat) %*% (X[i, ] - mu)
}
hist(d.2, col = 2, xlab = "Mahalanobis Distance")

X = sample[[2]]
d.2 = c()

for(i in 1:n[2]){
  d.2[i] = t(X[i, ] - mu) %*% solve(covmat) %*% (X[i, ] - mu)
}
hist(d.2, col=3, xlab = "Mahalanobis Distance")

X = sample[[3]]
d.2 = c()

for(i in 1:n[3]){
  d.2[i] = t(X[i, ] - mu) %*% solve(covmat) %*% (X[i, ] - mu)
}
hist(d.2, col = 4, xlab = "Mahalanobis Distance")

1cccc
library(mvtnorm)
library(stats4)
n = 1800
mu = c(11, 8, 12, 19, 10)
sigma = matrix(c(6, 4, 3, 2, 1, 4, 5, 4, 3, 2, 3, 4, 7, 4, 3, 2, 3, 4, 9, 4, 1, 2, 3, 4, 4), nrow = 5)


X <- rmvnorm(n, mu, sigma)
neg.ll = function(mean1,mean2,mean3,mean4,mean5, s11,s22,s33,s44,s55,s12,s13,s14,s15,s23,s24,s25,s34,s35,s45) {
  mean = c(mean1,mean2,mean3,mean4,mean5)
  S = matrix(
    c(s11, s12, s13, s14, s15,
      s12, s22, s23, s24, s25,
      s13, s23, s33, s34, s35,
      s14, s24, s34, s44, s45,
      s15, s25, s35, s45, s55
    ), 5, 5)
  log.ll = sum(dmvnorm(X, mean, S, log = TRUE))
  return(-log.ll)
}
xbar = colMeans(X)
S = var(X)


mle1 = mle(minuslogl=neg.ll, start=list(mean1=xbar[1],mean2=xbar[2],mean3=xbar[3],mean4=xbar[4],mean5=xbar[5],
                                        s11=S[1,1],s22=S[2,2],s33=S[3,3],s44=S[4,4],s55=S[5,5],
                                        s12=S[1,2],s13=S[1,3],s14=S[1,4],s15=S[1,5],
                                        s23=S[2,3],s24=S[2,4],s25=S[2,5],
                                        s34=S[3,4],s35=S[3,5],
                                        s45=S[4,5]))
mle1
