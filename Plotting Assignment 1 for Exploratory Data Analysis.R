library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

power_cons <- read.delim("household_power_consumption.txt",sep = ";",stringsAsFactors = FALSE)

power_cons$Date <- as.Date(power_cons$Date,format="%d/%m/%Y")
power_cons$Time <-  as.POSIXct(power_cons$Time,"%h:%m:%s")
power_cons <- subset(power_cons,Date == "2007-02-01" | Date == "2007-02-02")
sum(power_cons$Global_active_power=="?")
sum(power_cons$Global_reactive_power=="?")
sum(power_cons$Sub_metering_3=="?")
power_cons$Date_time <-  as.POSIXct(paste(power_cons$Date,power_cons$Time),format="%Y-%m-%d %H:%M:%S")

power_cons$day <-  wday(power_cons$Date,label = TRUE)
png("plot1.png")
hist(as.numeric(power_cons$Global_active_power) ,col = "red",xlab = "Global Active Power(kilowatts)",main="Global Active Power")
dev.off()


png("plot2.png")


power_cons_plot2 <- power_cons %>% group_by(day)
plot(power_cons_plot2$Date_time,power_cons_plot2$Global_active_power,type="l",xlab = "",ylab = "Global Active Power(kilowatts)")
dev.off()

power_cons_plot3 <- power_cons %>% gather(key=energy_sub_mtr,value=value,Sub_metering_1,Sub_metering_2,Sub_metering_3)
png("plot3.png")
with(power_cons_plot3, plot(Date_time, value, main = "",type = "l",ylab = "Energy sub metering",xlab = ""))
                      
with(subset(power_cons_plot3, energy_sub_mtr == "Sub_metering_1"), lines(Date_time, value, col = "black"))
with(subset(power_cons_plot3, energy_sub_mtr == "Sub_metering_2"), lines(Date_time, value, col = "red"))
with(subset(power_cons_plot3, energy_sub_mtr == "Sub_metering_3"), lines(Date_time, value, col = "blue"))

legend("topright", pch = 1, col = c("black", "red","blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))
dev.off()


png("plot4.png")
par(mfrow=c(2,2))
plot(power_cons_plot2$Date_time,power_cons_plot2$Global_active_power,type="l",xlab = "",ylab = "Global Active Power(kilowatts)")
plot(power_cons$Date_time,power_cons$Voltage,type="l",xlab = "datetime",ylab = "voltage")
with(power_cons_plot3, plot(Date_time, value, main = "",type = "l",ylab = "Energy sub metering",xlab = ""))

with(subset(power_cons_plot3, energy_sub_mtr == "Sub_metering_1"), lines(Date_time, value, col = "black"))
with(subset(power_cons_plot3, energy_sub_mtr == "Sub_metering_2"), lines(Date_time, value, col = "red"))
with(subset(power_cons_plot3, energy_sub_mtr == "Sub_metering_3"), lines(Date_time, value, col = "blue"))

legend("topright", pch = 1, col = c("black", "red","blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))
plot(power_cons_plot2$Date_time,power_cons_plot2$Global_reactive_power,type="l",xlab = "datetime",ylab = "Global reactive Power(kilowatts)")
dev.off()
