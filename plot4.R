# This function will produce four plots and write the to one png file.
# You may pass it a data frame as an argument.
# If you do not pass it an argument, it will fetch the data for the histogram by calling the 
# getData function, specified below.
# The point of this is that you may not want to reload the data every time you make a change 
# to this function.
makeMultiple <- function(df = NULL) {
        if (is.null(df)) {
                df <- getData()
        }
        # Make sure everything is in English
        Sys.setlocale("LC_TIME", "English")
        png(file="plot4.png")
        # Divide graphic device into two rows, two cols
        par(mfrow = c(2, 2))
        # Labels for each of the weekdays plus one day after that
        xlab <- c(df$Days[[1]], df$Days[[nrow(df)]], weekdays(df[[nrow(df), "Date"]] + 86400, abbreviate=TRUE))        

        # top left
        plot(df$Global_active_power, type="n", xlab="", ylab="Global Active Power", main="", xaxt="n")         
        lines(df$Global_active_power, type="l")
        axis(1, at = c(0, nrow(df)/2, nrow(df)), labels=xlab)
        
        # top right
        plot(df$Voltage, type="n", xlab="datetime", ylab="Voltage", main="", xaxt="n")         
        lines(df$Voltage, type="l")
        axis(1, at = c(0, nrow(df)/2, nrow(df)), labels=xlab)
        
        # bottom left
        plot(df$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering", main="", xaxt="n")         
        lcolors <- c("black", "red", "blue")
        lines(df$Sub_metering_1, col=lcolors[1], type="l")
        lines(df$Sub_metering_2, col=lcolors[2], type="l")
        lines(df$Sub_metering_3, col=lcolors[3], type="l")
        axis(1, at = c(0, nrow(df)/2, nrow(df)), labels=xlab)
        legend("topright", bty="n", lwd=1, col=lcolors, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        # bottom right
        plot(df$Global_reactive_power, type="n", xlab="datetime", ylab="Global_reactive_power", main="", xaxt="n")         
        lines(df$Global_reactive_power, type="l")
        axis(1, at = c(0, nrow(df)/2, nrow(df)), labels=xlab)
        
        dev.off()
}

# This function will download a file, turn its contents into a data frame
# and make a subset of this data frame for the two dates we are interested in.
# It also does some converting of the character string holding dates and times,
# turning them into POSIXlt objects.
getData <- function() {
        # Very helpfull: http://stackoverflow.com/a/3053883
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
        # Read the data
        powerData <- read.csv(unz(temp, "household_power_consumption.txt"), sep=";", na.strings="?", stringsAsFactors=FALSE)
        unlink(temp)
        # Make a subset
        data <- subset(powerData, (Date == "1/2/2007" | Date =="2/2/2007")) 
        dt <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
        days <- weekdays(dt, abbreviate=TRUE)
        # Make a new data frame with fewer cols
        pow <- data.frame(Date = dt, Days = days, data[,3:9], stringsAsFactors=FALSE)
        pow
}
