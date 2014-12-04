# This function will produce a graph with three lines and write it to a png file.
# You may pass it a data frame as an argument.
# If you do not pass it an argument, it will fetch the data for the histogram by calling the 
# getData function, specified below.
# The point of this is that you may not want to reload the data every time you make a change 
# to this function.
makeLines <- function(df = NULL) {
        if (is.null(df)) {
                df <- getData()
        }
        # Make sure everything is in English
        Sys.setlocale("LC_TIME", "English")
        png(file="plot3.png")
        plot(df$SM1, type="n", xlab="", ylab="Energy sub metering", main="", xaxt="n")         
        # Labels for each of the weekdays plus one day after that
        xlab <- c(df$Days[[1]], df$Days[[nrow(df)]], weekdays(df[[nrow(df), "Date"]] + 86400, abbreviate=TRUE))        
        lcolors <- c("black", "red", "blue")
        lines(df$SM1, col=lcolors[1], type="l")
        lines(df$SM2, col=lcolors[2], type="l")
        lines(df$SM3, col=lcolors[3], type="l")
        axis(1, at = c(0, nrow(df)/2, nrow(df)), labels=xlab)
        legend("topright", lwd=1, col=lcolors, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
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
        pow <- data.frame(Date = dt, Days = days, SM1 = data$Sub_metering_1, SM2 = data$Sub_metering_2, SM3 = data$Sub_metering_3, stringsAsFactors=FALSE)
        pow
}
