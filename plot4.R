
## Marc Tagne Assignment 1 : EDA COURSERA
##

## get the vector of classes to be assumed for the columns
testdata <- read.table("hpc.txt", header = TRUE , sep=";", na.strings = "?", comment.char = "", nrows = 100)
##get the columns' class
cclasses <- sapply(testdata, class)
#get rows 1/2/2007 to 2/2/2007 : Our overall goal 
## here is simply to examine how household energy
## usage varies over a 2-day period in February, 2007. 
## filename : hpc.txt
## sep : ";"

mydata <- read.table("hpc.txt", sep=";", col.names = names(cclasses), na.strings = "?", comment.char = "", colClasses = cclasses, skip=grep("31/1/2007;23:59:00", readLines("hpc.txt")), nrows = 1440*2, stringsAsFactors=FALSE)
##convert Date and Time : Join Date and Time column into a column name DateTime
#
mydata$DateTime <- as.POSIXct(paste(mydata$Date, mydata$Time), format="%d/%m/%Y %H:%M:%S")


##plot 2
plot2 <- function(filename=NULL){
  with(mydata, 
       plot(DateTime, Global_active_power, type = "l",xlab = "", ylab = "Global Active Power(killowatts)")
       )
      
      if(!missing(filename)){
        dev.copy(png,  file = filename)
        dev.off()
      }
}
 
##plot 3
plot3 <- function(filename=NULL){
  
  colors = c("black", "red", "blue")
  sub <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

  with(mydata, plot(DateTime, Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering"))
  
  for (i in 1:3){
    with(mydata, points(DateTime, mydata[, sub[i]], col= colors[i], type='l'))
  }

  
  if(!missing(filename)){
    legend("topright", pch = "---", col=colors, legend=sub, text.width = 60000, lty=1, xjust = 1, yjust=1)
    dev.copy(png,  file = filename)
    dev.off()
  }else{
    legend("topright", pch = "---", col=colors, legend=sub, text.width = 90000, lty=1, xjust = 1, yjust=1, bty = "n")
  }
}
 
##plot4
plot4 <- function(filename=NULL){
  
  par(mfrow = c(2,2))
  with(mydata, {
    plot2()
    plot(DateTime, Voltage,type="l")
    plot3()
    plot(DateTime, Global_reactive_power,type="l")
  })

  if(!missing(filename)){
    dev.copy(png,  file = filename)
    dev.off()
  }
}
## call  plot4 function 
plot4("plot4.png")