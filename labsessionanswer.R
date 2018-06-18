rm(list=ls())
cat("\14")
input<-read.csv("Dataset.csv")
voltage237<-subset(input, input$Voltage==237)
hist(voltage237$Global_active_power,main = "Global Active POwer",ylab = "Frequency",xlab = "Global Active power(KWh) for Voltage 237",col = "Red")
dev.copy(png,'plot1.png')
dev.off()

boxplot(voltage237$Global_active_power,main = "Global Active POwer",ylab = "Frequency",xlab = "Global Active power(KWh) for Voltage 237",col = "Red")
dev.copy(png,"plot2.png")
dev.off()

desiredval<-subset(input,input$Voltage>249)
plot(x=desiredval$Voltage,y=desiredval$Global_active_power,main = "Global Active POwer",ylab="Voltage",xlab="Global Active power(KWh) for Voltage 237", col = ifelse(desiredval$Voltage<250,'black','red'))

par(mfrow=c(3,1))
subset1<-subset(input,input$Sub_metering_2==0)
histss1<-hist(subset1$Global_active_power,col='Red',main="Global Active Power for sub metering=0.00",xlab = "Global active power",ylab = "Frequency")
subset2<-subset(input,input$Sub_metering_2==1)
histss2<-hist(subset2$Global_active_power,col='Red',main="Global Active Power for sub metering=1.00",xlab = "Global active power",ylab = "Frequency")
subset3<-subset(input,input$Sub_metering_2==2)
histss3<-hist(subset3$Global_active_power,col='Red',main="Global Active Power for sub metering=2.00",xlab = "Global active power",ylab = "Frequency")



    