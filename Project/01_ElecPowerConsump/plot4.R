# Load libraries, find/create folders, download and extract raw data, and read  
# into local tables
library(tidyverse); library(lubridate)

###### pre-name input directory
dir_proj <- "./Project"
dir_data <- paste(dir_proj, "/data", sep = "")

###### create variable and path names for UCI HAR zip file
hPowConsUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
hpc_zip <- "hPowCons.zip"
hpc_zip_pathname <- paste(dir_data, "/", hpc_zip, sep = "")

###### find & create project directories and input directories within
if(!file.exists(dir_proj)) { 
        dir.create(dir_proj)
}        
if(!file.exists(dir_data)) { 
        dir.create(dir_data)
}

###### download & extract data from zip archive
download.file(hPowConsUrl, 
              destfile = hpc_zip_pathname,
              method = "curl")
unzip(hpc_zip_pathname, exdir = dir_data)

###### load data into local data.frame
df_hpc <- read.table(paste(dir_data, "/", "household_power_consumption.txt", sep = ""),
                     header = TRUE, sep = ";", 
                     # colClasses = c("Date", "character", rep("numeric",7)),
                     stringsAsFactors = FALSE)
###### truncate data for only the dates "2007-02-01" and "2007-02-02" to reduce
###### occupation of RAM space and processing time
df_hpc <- filter(df_hpc, dmy(Date) >= "2007-02-01" & dmy(Date) <= "2007-02-02")
###### parse Date and Time fields into one DateTime field which plot functions
###### can interpret and order on a graph  
df_hpc$DateTime <- strptime(paste(df_hpc$Date, df_hpc$Time), "%d/%m/%Y %H:%M:%S")

###### check whether new variable is POSIXlt (i.e. is interpretable by plot)
wday(df_hpc$DateTime, label = TRUE); 
length(unique(df_hpc$DateTime)); length(unique(wday(df_hpc$DateTime)))
summary(wday(df_hpc$DateTime, label = TRUE))

###### typecast all fields that should have been loaded as numbers to numeric 
df_hpc[,3:8] <- lapply(df_hpc[,3:8], as.numeric)

# Create Plot 04
png(filename = "plot4.png", width = 480, height = 480)
par(mfcol = c(2, 2))
with(df_hpc, {
        plot(DateTime, Global_active_power, type = "l", lty = 1,
             xlab = "", ylab = "Global Active Power")
})
with(df_hpc, {
        plot(DateTime, Sub_metering_1, ylim = c(0, max(Sub_metering_1)), type = "l",
             xlab = "", ylab = "Energy sub metering")
        lines(DateTime, Sub_metering_2, type = "l", col = 'red')
        lines(DateTime, Sub_metering_3, type = "l", col = 'blue')
        legend("topright", legend = names(df_hpc)[7:9], bty = "n", 
               col=c('black', "red", "blue"), lty = 1, cex = 0.9, ncol = 1)
}) 
with(df_hpc, {
        plot(DateTime, Voltage, type = "l", lty = 1,
             xlab = "datetime", ylab = "Voltage")
})
with(df_hpc, {
        plot(DateTime, Global_reactive_power, type = "l", lty = 1,
             xlab = "datetime")
})
dev.off()
