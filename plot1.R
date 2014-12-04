# This function will produce a histogram and write it to a png file.
# You may pass it a data frame as an argument.
# If you do not pass it an argument, it will fetch the data for the histogram by calling the 
# getData function, specified below.
# The point of this is that you may not want to reload the data every time you make a change 
# to this function.
makeHist <- function(df = NULL) {
        if (is.null(df)) {
                df <- getData()
        }
        png(file="plot1.png")
        hist(df$Global_active_power, col="red", xlab="Global active power (kilowatts)", main="Global active power")
        dev.off()
}

# This function will download a file, turn its contents into a data frame
# and make a subset of this data frame for the two dates we are interested in.
getData <- function() {
        # Very helpfull: http://stackoverflow.com/a/3053883
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
        # Read the data
        powerData <- read.csv(unz(temp, "household_power_consumption.txt"), sep=";", na.strings="?", stringsAsFactors=FALSE)
        unlink(temp)
        # Make a subset
        smallPowerData <- subset(powerData, (Date == "1/2/2007" | Date =="2/2/2007")) 
        smallPowerData
}
