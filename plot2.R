# This function will produce a line graph and write it to a png file.
# You may pass it a data frame as an argument.
# If you do not pass it an argument, it will fetch the data for the histogram by calling the 
# getData function, specified below.
# The point of this is that you may not want to reload the data every time you make a change 
# to this function.
makeLine <- function(df = NULL) {
        if (is.null(df)) {
                df <- getData()
        }
        # Make sure everything is in English
        Sys.setlocale("LC_TIME", "English")
        png(file="plot2.png")
        plot(df$GAW, type="n", xlab="", ylab="Global active power (kilowatts)", main="Global active power", xaxt="n")         
        # Labels for each of the weekdays plus one day after that
        xlab <- c(df$Days[[1]], df$Days[[nrow(df)]], weekdays(df[[nrow(df), "Date"]] + 86400, abbreviate=TRUE))        
        lines(df$GAW, type="l")
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
        pow <- data.frame(Date = dt, Days = days, GAW = data$Global_active_power, stringsAsFactors=FALSE)
        pow
}
