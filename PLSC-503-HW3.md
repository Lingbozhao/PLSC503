PLSC 503 HW3
================
Lingbo ZHAO

## Question 1.1

The relationship between corr(X1, X2) and s.e.(*β̂*1) for N = 10

The code is as follows:

``` r
#install.packages("fMultivar")
library(fMultivar)
```

    ## Loading required package: timeDate

    ## Loading required package: timeSeries

    ## Loading required package: fBasics

``` r
set.seed(0914)

beta1_se<- NULL 
rho<- NULL

range <- seq(-1, 1, by=0.1)

for (i in 1:length(range))
{
  
  
  X<-cbind(rnorm2d(10,range[i]))
  
  Z<- as.data.frame(X)
  
  V3<- rnorm(10)
  
  Z$V3<- V3
  
  sig_2=1 
  u=rnorm(10,sd=sqrt(sig_2))
  
  beta0=2
  beta1=1
  beta2=2 
  beta3=3
  
  
  Z$y<- beta0+beta1*Z$V1+beta2*Z$V2+beta3*V3+u
  
  
  model<-lm(y~ V1+V2+V3, data = Z)
  
  summary(model)
  
  beta1_se_i=summary(model)$coef[2,2]
  rho_i= cor(Z$V1,Z$V2)
  
  
  beta1_se<-c(beta1_se,beta1_se_i)
  rho<-c(rho, rho_i)
}
plot(beta1_se,rho,main="Figure 1. Relationship between the SE of beta1 and correlation of X1X2 ",
      xlab="Standard error of estimated bata1", 
     ylab="Correlation of X1 and X2")
```

![](Figs/cars-1.png)<!-- -->

## Question 1.2

How that relationship changes as N → ∞

I set the N as 100, 1000, and 10000.

``` r
library(fMultivar)

set.seed(0914)


beta1_se<- NULL 
rho<- NULL

myrange <- seq(-1, 1, by=0.1)


for (i in 1:length(myrange))
{
  
  
  X<-cbind(rnorm2d(100,myrange[i]))
  
  Z<- as.data.frame(X)
  
  V3<- rnorm(100)
  
  Z$V3<- V3
  
  
  sig_2=1 
  u=rnorm(100,sd=sqrt(sig_2))
  
  beta0=2
  beta1=1
  beta2=2 
  beta3=3
  
  
  Z$y<- beta0+beta1*Z$V1+beta2*Z$V2+beta3*V3+u
  
  
  model<-lm(y~ V1+V2+V3, data = Z)
  
  summary(model)
  
  beta1_se_i=summary(model)$coef[2,2]
  rho_i= cor(Z$V1,Z$V2)
  beta1_i=model$coef[2]
  
  
  
  beta1_se<-c(beta1_se,beta1_se_i)
  rho<-c(rho, rho_i)
}

plot1= plot(beta1_se,rho,main="Figure 2. N=100 ",
      xlab="Standard error of estimated bata1", 
     ylab="Correlation of X1 and X2")
```

![](Figs/question12-1.png)<!-- -->

``` r
beta1_se<- NULL 
rho<- NULL

myrange <- seq(-1, 1, by=0.1)


for (i in 1:length(myrange))
{
  
  
  X<-cbind(rnorm2d(1000,myrange[i]))
  
  Z<- as.data.frame(X)
  
  V3<- rnorm(1000)
  
  Z$V3<- V3
  
  
  sig_2=1 
  u=rnorm(1000,sd=sqrt(sig_2))
  
  beta0=2
  beta1=1
  beta2=2 
  beta3=3
  
  
  Z$y<- beta0+beta1*Z$V1+beta2*Z$V2+beta3*V3+u
  
  
  model<-lm(y~ V1+V2+V3, data = Z)
  
  summary(model)
  
  beta1_se_i=summary(model)$coef[2,2]
  rho_i= cor(Z$V1,Z$V2)
  beta1_i=model$coef[2]
  
  
  
  beta1_se<-c(beta1_se,beta1_se_i)
  rho<-c(rho, rho_i)
}


plot2=plot(beta1_se,rho,main="Figure 3. N=1000 ",
      xlab="Standard error of estimated bata1", 
     ylab="Correlation of X1 and X2")
```

![](Figs/question12-2.png)<!-- -->

``` r
beta1_se<- NULL 
rho<- NULL

myrange <- seq(-1, 1, by=0.1)


for (i in 1:length(myrange))
{
  
  
  X<-cbind(rnorm2d(10000,myrange[i]))
  
  Z<- as.data.frame(X)
  
  V3<- rnorm(10000)
  
  Z$V3<- V3
  
  
  sig_2=1 
  u=rnorm(10000,sd=sqrt(sig_2))
  
  beta0=2
  beta1=1
  beta2=2 
  beta3=3
  
  
  Z$y<- beta0+beta1*Z$V1+beta2*Z$V2+beta3*V3+u
  
  
  model<-lm(y~ V1+V2+V3, data = Z)
  
  summary(model)
  
  beta1_se_i=summary(model)$coef[2,2]
  rho_i= cor(Z$V1,Z$V2)
  beta1_i=model$coef[2]
  
  
  
  beta1_se<-c(beta1_se,beta1_se_i)
  rho<-c(rho, rho_i)
}


plot3=plot(beta1_se,rho,main="Figure 4. N=10000 ",
      xlab="Standard error of estimated bata1", 
     ylab="Correlation of X1 and X2")
```

![](Figs/question12-3.png)<!-- -->

From the Figure 2 to the Figure 4,

![](Figs/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
