library(dplyr)                                                                                                                                                   
library(lubridate) 

v_all <- read.table("household_power_consumption.txt", sep=";", skip=1)                                                                                          
v_200702 <- v_all[ grep("^(0)?[1|2]/(0)?2/2007", v_all$V1), ]                                                                                                    
names(v_200702 ) <- c("v_Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")


XY200702 <-                                                                                                                                                      
  v_200702 %>%  
  mutate(dt_time=paste(v_Date,Time)) %>% 
  mutate(dt_time=dmy_hms(dt_time)) %>% 
  select( - (v_Date:Time)) %>% 
  
  mutate(Global_active_power=sub("\\?","NA",Global_active_power))  %>%   
  mutate(Global_reactive_power=sub("\\?","NA", Global_reactive_power))  %>% 
  mutate(Voltage=sub("\\?","NA", Voltage))  %>% 
  mutate(Sub_metering_1=sub("\\?","NA", Sub_metering_1))  %>% 
  mutate(Sub_metering_2=sub("\\?","NA", Sub_metering_2))  %>% 
  mutate(Sub_metering_3=sub("\\?","NA", Sub_metering_3))  %>% 
  
  mutate(Global_active_power=as.numeric(Global_active_power, na.rm=TRUE))  %>%
  mutate(Global_reactive_power=as.numeric(Global_reactive_power, na.rm=TRUE))  %>% 
  mutate(Voltage=as.numeric(Voltage, na.rm=TRUE))  %>% 
  mutate(Sub_metering_1=as.numeric(Sub_metering_1, na.rm=TRUE))  %>% 
  mutate(Sub_metering_2=as.numeric(Sub_metering_2, na.rm=TRUE))  %>% 
  mutate(Sub_metering_3=as.numeric(Sub_metering_3, na.rm=TRUE)) 

png("plot4.png", width=480, height = 480)
#par(mfrow = c(1, 3), mfcol=c(2,4), mar=c(1,2,2,1), oma=c(0,0,0,0))
par(mfrow = c(2, 2), mar=c(4,4,2,2) +0.1, oma = c(0, 0, 2, 0))
# subplot1
with(XY200702, {
  plot(dt_time,Global_active_power, type="l", xlab="", ylab="Global Active Power")
  
  plot(dt_time, Voltage,  type="l", xlab = "datetime", ylab="Voltage")
  
  plot(dt_time, Sub_metering_1,  type="l", xlab="", ylab="Energy sub metering")
  #lines(dt_time, Sub_metering_1)
  lines(dt_time, Sub_metering_2, col="red")  
  lines(dt_time, Sub_metering_3, col="blue")   
  legend("topright",pch= "-",  col = c("black","red", "blue"), y.intersp=1.2, xjust=1, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3") )
  
  plot(dt_time, Global_reactive_power,  type="l", xlab= "datetime", ylab="Global_reactive_power")
  
  title("Plot4", outer=TRUE, adj=0)
}
)

dev.off()

