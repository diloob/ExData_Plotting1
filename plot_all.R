## Course Project Assignment: Exploratory Data Analysis, Project 1 

library(data.table)
library(lubridate)
library(dplyr)

datafileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dataZipfile <- "household_power_consumption.zip"
datafile <- "household_power_consumption.txt"

if (!file.exists(dataZipfile)) {
	download.file(datafileUrl, dataZipfile, mode = "wb")
}

# Unzip the file if the target file is not present
if (is.na(file.info(datafile)$isdir)) {
	unzip(dataZipfile)	
}

# Read the data for 01-Feb-2007 and 02-Feb-2007
filehandle <- file(datafile)

consumption.data <- read.table(
			text = grep("^[1,2]/2/2007", readLines(filehandle), value = TRUE),
			col.names = c("Date", "Time", 
						"Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
						"Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
			colClasses = c("character","character",
						'numeric','numeric','numeric','numeric',
						'numeric','numeric','numeric'),
			sep = ";",
			header = FALSE,
			na.strings="?")

# close the file
close(filehandle)

# Remove incomplete observation
consumption.data <- consumption.data[complete.cases(consumption.data),]

# Add a concatenated POSIXct field (concatenated from date and time) 
consumption.data <- mutate(consumption.data, datetime=as.POSIXct(paste(Date,Time), format="%d/%m/%Y %H:%M:%S"))

# Remove Date and Time column.  These columns are redundant
#consumption.data <- consumption.data[ ,!(names(consumption.data) %in% c("Date","Time"))]
 
# Generate Plot1
hist(consumption.data$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

# Saving to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

# Generate Plot2
plot(consumption.data$Global_active_power ~ consumption.data$datetime, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")

# Saving to file
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

# Generate Plot3
with(consumption.data, {
			plot(Sub_metering_1  ~ datetime, type="l", ylab = "Energy sub metering", xlab="")
			lines(Sub_metering_2 ~ datetime,col='Red')
			lines(Sub_metering_3 ~ datetime,col='Blue')
			legend("topright",
					col=c("black", "red", "blue"), lwd=2, 
					legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
				)
			}
		)

# Saving to file
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

# Generate Plot4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(consumption.data, {
			plot(Global_active_power   ~ datetime, type="l", ylab="Global Active Power", xlab="")
			plot(Voltage               ~ datetime, type="l", ylab="Voltage",             xlab="datetime")
			plot(Sub_metering_1        ~ datetime, type="l", ylab="Energy sub metering", xlab="")
				lines(Sub_metering_2       ~ datetime, col='Red')
				lines(Sub_metering_3       ~ datetime, col='Blue')
				legend("topright",
						col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
						legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
				)
			plot(Global_reactive_power ~ datetime, type="l", xlab="datetime")
			}
		)

# Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
