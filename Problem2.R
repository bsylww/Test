library(openxlsx)
library(ggplot2)
library(foreign)
library(outliers)
library(astsa)

##Load data
d <- read.table("/Users/WW/Downloads/EagleFord_Production.txt", 
               sep="\t", header=TRUE, fill=TRUE, fileEncoding="UTF-16")

d$api <- as.factor(d$api)
d$date <- as.Date(d$date, "%m/%d/%Y")

mydf <- read.xlsx("/Users/WW/Desktop/Well.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
mydf$api <- as.factor(mydf$api)
mydf$operator_name <- as.factor(mydf$operator_name)
##merge table to get company information
ddd <- merge(d, mydf, by="api", all.x=TRUE)
##extract useful data
production <- ddd[c("date", "operator_name", "volume_oil_formation_bbls", "volume_gas_formation_mcf")]

##determine historical production results by company.
p <- aggregate(. ~ date+operator_name,data = production,FUN=sum)

##Compute historical mean and standard deviation by company
##Based on mean and standard deviation
##The company with higher mean and less standard deviation will be successful
pp <- p[c("operator_name", "volume_oil_formation_bbls", "volume_gas_formation_mcf")]
ppp <- aggregate(. ~ operator_name,data = pp,FUN=function(x) c(mn =mean(x), sd=sd(x) ) )

plot(x=ppp$volume_oil_formation_bbls[,1], y=ppp$volume_oil_formation_bbls[,2], xlab="Mean", ylab="SD")
plot(x=ppp$volume_gas_formation_mcf[,1], y=ppp$volume_gas_formation_mcf[,2], xlab="Mean", ylab="SD", 
     xlim=c(0,50000), ylim=c(0,50000))

##plot different company production according to date
ggplot(p, aes(date, volume_oil_formation_bbls, colour=operator_name)) + 
  scale_x_date(limits = as.Date(c('2010-01-01','2015-12-01')))+
  guides(colour=FALSE)+
  geom_line() + 
  geom_point()

ggplot(p, aes(date, volume_gas_formation_mcf, colour=operator_name)) + 
  scale_x_date(limits = as.Date(c('2010-01-01','2015-12-01')))+
  guides(colour=FALSE)+
  geom_line() + 
  geom_point()

##Find outliers
row_sub = which(p$volume_gas_formation_mcf==0)
p <- p[-row_sub,]
row_sub = which(p$volume_oil_formation_bbls==0)
p <- p[-row_sub,]
pp <- p[c("operator_name", "volume_oil_formation_bbls", "volume_gas_formation_mcf")]
p_sum <- aggregate(. ~ operator_name,data = pp,FUN=sum)
plot(p_sum$volume_oil_formation_bbls, p_sum$volume_gas_formation_mcf)
value <- outlier(p_sum$volume_oil_formation_bbls, opposite = FALSE, logical = FALSE)
p_sum <- p_sum[which(p_sum$volume_oil_formation_bbls!=value),]
value <- outlier(p_sum$volume_gas_formation_mcf, opposite = FALSE, logical = FALSE)
which(p_sum$volume_gas_formation_mcf==value)
p_sum <- p_sum[which(p_sum$volume_gas_formation_mcf!=value),]
##Best company by productio volumn
p_sum[which.max(p_sum$volume_oil_formation_bbls),1]
p_sum[which.max(p_sum$volume_gas_formation_mcf),1]

##In order to find who is most likely to succeed in the future when drilling wells
##Use AR(1) model to predict production
##For every company:
##1.plot ACF
##2.fit using AR(1) model
##3.predict in future 8 time point
company <- as.factor(levels(p$operator_name))
name <- rep(NA,length(company))

for(i in 1:length(company))
{
  
  plot_data <- p[p$operator_name==company[i],]
  if(is.data.frame(plot_data) && nrow(plot_data)>=12)
  {
    par(mfrow=c(2,2))
    print(plot_data$operator_name)
    plot(plot_data$date,plot_data$volume_oil_formation_bbls, type="l")
    acf(plot_data$volume_oil_formation_bbls, lag.max=20)
    fit<-arima(plot_data$volume_oil_formation_bbls,order=c(1,0,0))
    tsdiag(fit)
    pred<-predict(fit,n.ahead=8)
    print(pred)
    plot(plot_data$volume_oil_formation_bbls, xlim=c(0, length(plot_data$volume_oil_formation_bbls)+10))
    lines(pred$pred,col="blue", lwd=5)
    lines(pred$pred+2*pred$se,col="red",lty=3, lwd=5)
    lines(pred$pred-2*pred$se,col="red",lty=3, lwd=5)
  }
}
