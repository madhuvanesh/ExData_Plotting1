## This function generates the Plot 3 from the assignment 1
## of Energy sub metering to Day of week for the 2 days in Feb, 2007. 
## This function will also create plot3.png in the current working directory
## The parameter inputfile: The complete or relative path to the input file, including file name. This is the file
## downloaded from the URL in the assignment page

plot4 <- function (inputfile){
tryCatch({
# try/catch in case the input file is invalid
	## read.table() call passing input file, header=TRUE since the file is expected
	## to have the header, sep=";" as this is what is in the file,
	## na.strings = "?" as specified in the assignment

	electric_power <- read.table(inputfile,header=TRUE,sep=";",na.strings="?")
	# add an actual date column (Date2) derived from the Date column which is a string in the "dd/mm/yyyy" format
	electric_power$Date2 <- as.Date(electric_power$Date,"%d/%m/%Y") # Date2 will be in the YYYY-MM-DD format
	# subset to get data only for 1st Feb to 2nd Feb, 2007
	electric_power_subset <- subset(electric_power,Date2 == "2007-02-01" | Date2 == "2007-02-02")

	# use mfcol to set the columns and rows in the plot area
	par(mfcol = c(2,2))
# draw Global active power chart
	
	# use plot(), turning off x axis labels through xaxt and using type="n" to NOT render yet
	with(electric_power_subset,plot(Global_active_power,ylab="Global Active Power",xlab="",xaxt="n",type="n"))
	with(electric_power_subset,lines(Global_active_power))
	
	# now render the custom X-axis with Thu to Sat. This is since the dates:
	# 2007-02-01 and 2007-02-02 are Thu and Fri respectively. Add Sat as specified in assignment
	# the X- axis labels are place at 1, mid and end using nrow() to compute them
	axis(1,at=c(1,nrow(electric_power_subset)/2,nrow(electric_power_subset)),
		labels=c("Thu","Fri","Sat"))

# draw energy sub-metering
	# use plot(). But turn off rendering, since we want to use lines() instead of using the default plotting
	with(electric_power_subset,plot(Sub_metering_1,ylab="Energy sub metering",xlab="",xaxt="n",type="n",ylim=c(0,40)))
	# Now draw the lines
	with(electric_power_subset,lines(Sub_metering_1))

	# Now draw the lines
	with(electric_power_subset,lines(Sub_metering_2,col="red",ylim=c(0:30)))

	# Now draw the lines
	with(electric_power_subset,lines(Sub_metering_3,col="blue"))
	
	# the X- axis labels are place at 1, mid and end using nrow() to compute them
	axis(1,at=c(1,nrow(electric_power_subset)/2,nrow(electric_power_subset)),
		labels=c("Thu","Fri","Sat"))

	# now, draw the legend on topright
	legend(x="topright",pch=1,legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"))
	
#draw Voltage
	# use plot(), turning off x axis labels through xaxt and using type="n" to NOT render yet
	with(electric_power_subset,plot(Voltage,xlab="datetime",xaxt="n",type="n"))
	with(electric_power_subset,lines(Voltage))
	
	# now render the custom X-axis with Thu to Sat. This is since the dates:
	# 2007-02-01 and 2007-02-02 are Thu and Fri respectively. Add Sat as specified in assignment
	# the X- axis labels are place at 1, mid and end using nrow() to compute them
	axis(1,at=c(1,nrow(electric_power_subset)/2,nrow(electric_power_subset)),
		labels=c("Thu","Fri","Sat"))


#draw global reactive power
	# use plot(), turning off x axis labels through xaxt and using type="n" to NOT render yet
	with(electric_power_subset,plot(Global_reactive_power,xlab="datetime",xaxt="n",type="n"))
	with(electric_power_subset,lines(Global_reactive_power))
	
	# now render the custom X-axis with Thu to Sat. This is since the dates:
	# 2007-02-01 and 2007-02-02 are Thu and Fri respectively. Add Sat as specified in assignment
	# the X- axis labels are place at 1, mid and end using nrow() to compute them
	axis(1,at=c(1,nrow(electric_power_subset)/2,nrow(electric_power_subset)),
		labels=c("Thu","Fri","Sat"))

	# copy to png file in the current working directory
	dev.copy(png,file="plot4.png")
	# close the png device
	dev.off()
}, warning = function(w){
	warning(w)
}, error = function(e) {
	stop(e)
}, finally = {
})
}
