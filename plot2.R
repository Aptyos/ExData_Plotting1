library(dplyr)
library(lubridate)
v_all <- read.table("household_power_consumption.txt", sep=";", skip=1)
v_200702 <- v_all[ grep("^(0)?[1|2]/(0)?2/2007", v_all$V1), ]
names(v_200702) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

XV200702 <- 
  v_200702 %>%
  mutate(dt_time=paste(Date,Time)) %>%
  mutate(dt_time=dmy_hms(dt_time)) %>%
  select( - (Date:Time)) %>%
  mutate(Global_active_power=as.numeric(Global_active_power, na.rm=TRUE))  %>%
  mutate(Global_active_power=Global_active_power/500)

png("plot2.png", width=480, height = 480)
with(XV200702, plot(dt_time,Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts"))
title("Plot 2", adj=0)
dev.off()
