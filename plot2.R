## Marc Tagne Assignment 1,plot 2 : EDA COURSERA
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
## plot2 function call
plot2("plot2.png")