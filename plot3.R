library(dplyr)                                                                                                                                                   
library(lubridate)                                                                                                                                               
#library(ggplot2)                                                                                                                                                 
v_all <- read.table("household_power_consumption.txt", sep=";", skip=1)                                                                                          
v_200702 <- v_all[ grep("^(0)?[1|2]/(0)?2/2007", v_all$V1), ]                                                                                                    
names(v_200702 ) <- c("v_Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")


XY200702 <-                                                                                                                                                      
  v_200702 %>%  
  mutate(dt_time=paste(v_Date,Time)) %>% 
  mutate(dt_time=dmy_hms(dt_time)) %>% 
  select( - (v_Date:Time)) %>%                                                                                                                                     
  mutate(Global_active_power=as.numeric(Global_active_power, na.rm=TRUE))  %>%   
  mutate(Sub_metering_1=sub("\\?","NA", Sub_metering_1))  %>% 
  mutate(Sub_metering_2=sub("\\?","NA", Sub_metering_2))  %>% 
  mutate(Sub_metering_3=sub("\\?","NA", Sub_metering_3))  %>% 
  
  mutate(Sub_metering_1=as.numeric(Sub_metering_1, na.rm=TRUE))  %>% 
  mutate(Sub_metering_2=as.numeric(Sub_metering_2, na.rm=TRUE))  %>% 
  mutate(Sub_metering_3=as.numeric(Sub_metering_3, na.rm=TRUE))  %>% 
  mutate(Global_active_power=Global_active_power/500)  

png("plot3.png", width=480, height = 480)
plot(XY200702$dt_time, XY200702$Sub_metering_1,  type="n", xlab ="", ylab="Energy sub metering")
lines(XY200702$dt_time, XY200702$Sub_metering_1)
lines(XY200702$dt_time, XY200702$Sub_metering_2, col="red")  
lines(XY200702$dt_time, XY200702$Sub_metering_3, col="blue")   
legend("topright",pch= "-",  col = c("black","red", "blue"), y.intersp=1.2, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3") )
title("Plot 3", adj=0)
dev.off()


