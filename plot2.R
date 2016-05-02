## This function generates the Plot 2 from the assignment 1
## of Global Active Power to Day of week for the 2 days in Feb, 2007. 
## This function will also create plot2.png in the current working directory
## The parameter inputfile: The complete or relative path to the input file, including file name. This is the file
## downloaded from the URL in the assignment page

plot2 <- function (inputfile){
tryCatch({

	# use mfcol to set to 1 col and 1 row since only one diagram will be displayed
	par(mfcol = c(1,1))
# try/catch in case the input file is invalid
	## read.table() call passing input file, header=TRUE since the file is expected
	## to have the header, sep=";" as this is what is in the file,
	## na.strings = "?" as specified in the assignment

	electric_power <- read.table(inputfile,header=TRUE,sep=";",na.strings="?")
	# add an actual date column (Date2) derived from the Date column which is a string in the "dd/mm/yyyy" format
	electric_power$Date2 <- as.Date(electric_power$Date,"%d/%m/%Y") # Date2 will be in the YYYY-MM-DD format
	
	# subset to get data only for 1st Feb to 2nd Feb, 2007
	electric_power_subset <- subset(electric_power,Date2 == "2007-02-01" | Date2 == "2007-02-02")
		
	# use plot(), turning off x axis labels through xaxt and using type="n" to NOT render yet
	with(electric_power_subset,plot(Global_active_power,ylab="Global Active Power(kilowatts)",xlab="",xaxt="n",type="n"))
	with(electric_power_subset,lines(Global_active_power))
	
	# now render the custom X-axis with Thu to Sat. This is since the dates:
	# 2007-02-01 and 2007-02-02 are Thu and Fri respectively. Add Sat as specified in assignment
	# the X- axis labels are place at 1, mid and end using nrow() to compute them
	axis(1,at=c(1,nrow(electric_power_subset)/2,nrow(electric_power_subset)),
		labels=c("Thu","Fri","Sat"))
	
	#title(ylab="Global Active Power(kilowatts)")
	#title(xlab='') # turnoff the x-axis label
	#mtext("Global Active Power(kilowatts)",side=2)
	#mtext("",side=1)

	# copy to png file in the current working directory
	dev.copy(png,file="plot2.png")
	# close the png device
	dev.off()
}, warning = function(w){
	warning(w)
}, error = function(e) {
	stop(e)
}, finally = {
})
}
