library(data.table)
data<-data.table::fread("household_power_consumption.txt",header=TRUE,na.strings = "?",sep=";")
data$Date<-as.Date(data$Date,"%d/%m/%Y")
data1<-subset(data,Date>=as.Date("2007-2-1") & Date<=as.Date("2007-2-2"))
dateTime<-paste(data1$Date,data1$Time)
dateTime <- setNames(dateTime, "DateTime")
data1<- cbind(dateTime, data1)
data1$dateTime <- as.POSIXct(dateTime)
data1=subset(data1,select=-c(2,3))
png(file="plot4.png",width=480,height=480)
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data1, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage", xlab="dateTime")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Energy sub metering", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global_rective_power",xlab="dateTime")
})
dev.off()
