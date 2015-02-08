# NAME : openDataset
# INPUT : Open the file in the directory
#   The directory should contains the file to open 
#       ....
# OUTPUT : A dataframe with the required data extracted from the file
#
# COMMENTS :
#   Raw Data from files 
#
library(datasets)

LoadDataset <- function(directory,file) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the  file
    d<-getwd()
    if (!is.null(directory)) {
        setwd(directory)
    }
    else {
        printf("Invalid directory!")
        return(1)
    }
    print(directory);    
    #  
    df = read.csv(file, sep=";", header = TRUE); 
    #
    #dim(df)
    df$Date2 <- as.Date(as.character(df$Date), "%d/%m/%Y")
    df$Time2 <- strptime(df$Time, "%H:%M:%S")
    #ds <- subset(df, Date2 > as.Date("2007-01-13") )
    ds <- subset(df, Date2 >= as.Date("2007-02-01") & Date2 <= as.Date("2007-02-02"))   
    setwd(d)
    return(ds)
}


# NAME : Plot3
# INPUT : A dataframe with the data to plot
#       ....
# OUTPUT : A Png file with the statistics
#
# COMMENTS :
#   A similar image ca be found in the GitHub repository . 
#   We must reconstruct it from scratch from the Data set
#
Plot3 <- function(df) {
    dateTime<- as.POSIXlt(paste(as.Date(df$Date, format="%d/%m/%Y"), df$Time, sep=" "))
    with(df, plot(dateTime,as.numeric(as.character(Sub_metering_1)), type = "l", col = "black",xlab = "", ylab = "Energy Sub metering")) ## Create plot on screen device
    lines(dateTime,as.numeric(as.character(df$Sub_metering_2)), col = "red") ## Create plot on screen device
    lines(dateTime,as.numeric(as.character(df$Sub_metering_3)), col = "blue") ## Create plot on screen device
    legend("topright",pch=45,col=c("black","red","blue"),legend=c("Sub_metering_1    ","Sub_metering_2    ","Sub_metering_3    "), box.lty = 1, inset = .002)
    
    dev.copy(png, file = "plot3.png", width = 480, height = 480) ## Copy my plot to a PNG file
    dev.off() ## Don't forget to close the PNG device!    
}

dev.off()
wd<-"C:/FORMATIONS/MOOC/COURSERA/Data Science Specialization/EXPLORATORY DATA ANALYSIS/PROJECT1/"
f<-"household_power_consumption.txt"
ds<-LoadDataset(wd,f)
Plot3(ds)
dev.off()

