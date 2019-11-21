library(fBasics)
library(forecast)

data<-read.csv("C:/Users/Usuario/Desktop/term 2 MBD/Time series/group assignment/Homework_1_DATA.csv",header=TRUE,sep=";",dec=",")


series1=data[,1] [1:300]
series2=data[,2] [1:300]
series3=data[,3] [1:300]
series4=data[,4] [1:300]
series5=data[,5] [1:2000]
series6=data[,6]
series7=data[,7]



### question 1

plot_function <- function(series) {
  ts.plot(series7)   
  acf(series7)
  pacf(series7)
  hist(series,prob=T,xlim=c(mean(y)-3*sd(series),mean(series)+3*sd(series)),col="red")
  lines(density(series),lwd=2)
  mu<-mean(series)
  sigma<-sd(series)
  x<-seq(mu-3*sigma,mu+3*sigma,length=100)
  yy<-dnorm(x,mu,sigma)
  lines(x,yy,lwd=2,col="blue")
  
}

list_series<-list(series1,series2)
for (i in list_series){
  
plot_function(i)  
}


