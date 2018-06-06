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
