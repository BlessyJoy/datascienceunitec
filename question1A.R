rm(list=ls())
cat("\14")
library(dplyr)
setwd("C:/Users/Blessy Edward/Desktop/ESR/html files from darknet/ESR")
data<-read.csv("Data Set 6.csv")
levels(data$City)<-c(levels(data$City),"Auckland ")
data$City[data$City=="Acklnad"]<- "Auckland "
data1<- data %>% filter(data$City== "Wellington" | data$City=="Auckland ")
data_opt<-filter(data1,data1$P.Winter>0 & data1$P.Winter<=2500,data1$P.Summer>0 & data1$P.Summer<2500,data1$Area>0 & data1$Area<=300)
#annual_avg<-function(data_opt)

  data_opt$P.Annual<-(data_opt$P.Winter+data_opt$P.Summer)/2

#PART B
#Calculating mean & SD For P.summer
mean_summer<-mean(data_opt$P.Summer)
sd_summer<-sd(data_opt$P.Summer)

#Calculating mean & SD For P.winter
mean_winter<-mean(data_opt$P.Winter)
sd_winter<-sd(data_opt$P.Winter)


#Calculating mean & SD For P.Annual
mean_annual<-mean(data_opt$P.Annual)
sd_Annual<-sd(data_opt$P.Annual)


#Density Function for Winter Adjust Scaling
den1<-density(data_opt$P.Winter)
plot(x=den1,main = "Density Function for Power COnsumpution During Winter",xlab = "KWh",ylab = "Density Factor")

#Density Function for Summer Adjust Scaling
den2<-density(data_opt$P.Summer)
plot(x=den2,main = "Density Function for Power COnsumpution During Summer",xlab = "KWh",ylab = "Density Factor")

#Density Function for Annaul Usage Adjust Scaling
den3<-density(data_opt$P.Annual)
plot(x=den3,main = "Density Function for Power COnsumpution",xlab = "KWh",ylab = "Density Factor")

#BOxplot for Summer, winter and annual usuage
boxplot(data_opt$P.Summer,data_opt$P.Winter,data_opt$P.Annual,xlab="Power Consumption During summer, winter and annual average use",ylab="KWh")

#forming Subsets based on city
auck<-subset(data_opt,data_opt$City=='Auckland ')
well<-subset(data_opt,data_opt$City=='Wellington')

# Finding mean and SD for auckland
meanaucksummerr<-mean(auck$P.Summer)
meanauckwinter<-mean(auck$P.Winter)
meanauckavg<-mean(auck$P.Annual)

#finidng Mean and SD for Wellington
meanwellsummerr<-mean(well$P.Summer)
meanwellwinter<-mean(well$P.Winter)
meanwellavg<-mean(well$P.Annual)


#Creating Table
result<-matrix(nrow = 9,ncol = 2)
colnames(result)<-c("Mean","Standard Deviation")
rownames(result)<-c("Power Consumption During Winter","Power Consumption During Summer","Annual Power Consumption ","Power Consumption During Winter in Auckland","Power Consumption During Summer in Auckland","Annual Power Consumption in Auckland ","Power Consumption During Winter in wellington","Power Consumption During Summer in wellington","Annual Power Consumption in wellington")

result[1,1]<-mean_winter
result[2,1]<-mean_summer
result[3,1]<-mean_annual
result[4,1]<-meanauckwinter
result[5,1]<-meanaucksummerr
result[6,1]<-meanauckavg
result[7,1]<-meanwellwinter
result[8,1]<-meanwellsummerr
result[9,1]<-meanwellavg
result[1,2]<-sd_winter
result[2,2]<-sd_summer
result[3,2]<-sd_Annual
result[4,2]<-sd(auck$P.Winter)
result[5,2]<-sd(auck$P.Summer)
result[6,2]<-sd(auck$P.Annual)
result[7,2]<-sd(well$P.Winter)
result[8,2]<-sd(well$P.Summer)
result[9,2]<-sd(well$P.Annual)

#Scatterplot
plot(x=data_opt$Area,y=data_opt$P.Annual,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(data_opt$P.Annual~data_opt$Area)

ggplot(data_opt, aes(y=data_opt$P.Annual, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(data_opt$P.Annual ~ poly(Area, 2, raw = TRUE),data= data_opt)

#Scatter PLot
ggplot(data_opt, aes(y=data_opt$P.Annual, x=data_opt$Area)) + 
      geom_point(alpha = .5) + 
     stat_smooth(method = "lm", formula = y ~ poly(x,2))
mean(model1$residuals^2)

#3rd order ploynomial regressio
model2<-lm(data_opt$P.Annual ~ poly(Area, 2, raw = TRUE),data= data_opt)

#plotting 3rd order regression
ggplot(data_opt, aes(y=data_opt$P.Annual, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))

mean(model2$residuals^2)
#######################################################################
#p.winter
#######################################

#Scatterplot
plot(x=data_opt$Area,y=data_opt$P.Winter,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(data_opt$P.Winter~data_opt$Area)

ggplot(data_opt, aes(y=data_opt$P.Winter, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ x)

#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(data_opt$P.Winter ~ poly(Area, 2, raw = TRUE),data= data_opt)

#Scatter PLot
ggplot(data_opt, aes(y=data_opt$P.Winter, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))

mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(data_opt$P.Winter ~ poly(Area, 2, raw = TRUE),data= data_opt)

#plotting 3rd order regression
ggplot(data_opt, aes(y=data_opt$P.Winter, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))

mean(model2$residuals^2)
#########################################################################################
#p.Summer
########################################################################################

#Scatterplot
plot(x=data_opt$Area,y=data_opt$P.Summer,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(data_opt$P.Summer~data_opt$Area)

ggplot(data_opt, aes(y=data_opt$P.Summer, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ x)

#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(data_opt$P.Summer ~ poly(Area, 2, raw = TRUE),data= data_opt)

#Scatter PLot
ggplot(data_opt, aes(y=data_opt$P.Summer, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))

mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(data_opt$P.Summer ~ poly(Area, 2, raw = TRUE),data= data_opt)

#plotting 3rd order regression
ggplot(data_opt, aes(y=data_opt$P.Summer, x=data_opt$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))
mean(model2$residuals^2)
###############################################################################################################################################
#Auckland
####################################################

#Scatterplot
plot(x=auck$Area,y=auck$P.Annual,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(auck$P.Annual~auck$Area)

ggplot(auck, aes(y=auck$P.Annual, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(auck$P.Annual ~ poly(Area, 2, raw = TRUE),data= auck)

#Scatter PLot
ggplot(auck, aes(y=auck$P.Annual, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(auck$P.Annual ~ poly(Area, 2, raw = TRUE),data= auck)

#plotting 3rd order regression
ggplot(auck, aes(y=auck$P.Annual, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))

mean(model2$residuals^2)
#######################################################################
#p.winter
#######################################

#Scatterplot
plot(x=auck$Area,y=auck$P.Winter,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(auck$P.Winter~auck$Area)

ggplot(auck, aes(y=auck$P.Winter, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ x)

#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(auck$P.Winter ~ poly(Area, 2, raw = TRUE),data= auck)

#Scatter PLot
ggplot(auck, aes(y=auck$P.Winter, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(auck$P.Winter ~ poly(Area, 2, raw = TRUE),data= auck)

#plotting 3rd order regression
ggplot(auck, aes(y=auck$P.Winter, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))

mean(model2$residuals^2)
#########################################################################################
#p.Summer
########################################################################################

#Scatterplot
plot(x=auck$Area,y=auck$P.Summer,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(auck$P.Summer~auck$Area)

ggplot(auck, aes(y=auck$P.Summer, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ x)

#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(auck$P.Summer ~ poly(Area, 2, raw = TRUE),data= auck)

#Scatter PLot
ggplot(auck, aes(y=auck$P.Summer, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(auck$P.Summer ~ poly(Area, 2, raw = TRUE),data= auck)

#plotting 3rd order regression
ggplot(auck, aes(y=auck$P.Summer, x=auck$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))
mean(model2$residuals^2)
###########################################################################################################################################
#well
####################################################################
#Scatterplot
plot(x=well$Area,y=well$P.Annual,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(well$P.Annual~well$Area)

ggplot(well, aes(y=well$P.Annual, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(well$P.Annual ~ poly(Area, 2, raw = TRUE),data= well)

#Scatter PLot
ggplot(well, aes(y=well$P.Annual, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(well$P.Annual ~ poly(Area, 2, raw = TRUE),data= well)

#plotting 3rd order regression
ggplot(well, aes(y=well$P.Annual, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))
mean(model2$residuals^2)
#######################################################################
#p.winter
#######################################

#Scatterplot
plot(x=well$Area,y=well$P.Winter,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(well$P.Winter~well$Area)

ggplot(well, aes(y=well$P.Winter, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ x)

#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(well$P.Winter ~ poly(Area, 2, raw = TRUE),data= well)

#Scatter PLot
ggplot(well, aes(y=well$P.Winter, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))
mean(model1$residuals^2)
#3rd order ploynomial regressio
model2<-lm(well$P.Winter ~ poly(Area, 2, raw = TRUE),data= well)

#plotting 3rd order regression
ggplot(well, aes(y=well$P.Winter, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))
mean(model2$residuals^2)
#########################################################################################
#p.Summer
########################################################################################

#Scatterplot
plot(x=well$Area,y=well$P.Summer,main = "Scatterplot for power consupmtion",xlab= "Location",ylab="Power Consumption in KWh",col="Blue",pch=19)

#linear model
model<-lm(well$P.Summer~well$Area)

ggplot(well, aes(y=well$P.Summer, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ x)

#MSE
mean(model$residuals^2)

#Second Order POlynomial Regression
model1<-lm(well$P.Summer ~ poly(Area, 2, raw = TRUE),data= well)

#Scatter PLot
ggplot(well, aes(y=well$P.Summer, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,2))

mean(model1$residuals^2)
#3rd order ploynomial regressio
model1<-lm(well$P.Summer ~ poly(Area, 2, raw = TRUE),data= well)

#plotting 3rd order regression
ggplot(well, aes(y=well$P.Summer, x=well$Area)) + 
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x,3))
mean(model2$residuals^2)

