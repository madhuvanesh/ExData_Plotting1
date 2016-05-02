## This function generates the histogram (Plot 1 from the assignment page)
## of Frequency to Global Active Power. This function will also create plot1.png in the current working directory
## The parameter inputfile: The complete or relative path to the input file, including file name. This is the file
## downloaded from the URL in the assignment page

plot1 <- function (inputfile){
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
	
	# use hist() to generate the histogram for Global Active Power
	# Parameters main, xlab(for label on X axis) and col (for color) are set appropriately.
	# No ylab is specified so leave the Y axis label as default to "Frequency"
	hist(electric_power_subset$Global_active_power,main="Global Active Power",xlab="Global Active Power(kilowatts)",col="red")
	
	# copy to png file in the current working directory
	dev.copy(png,file="plot1.png")
	# close the png device
	dev.off()
}, warning = function(w){
	warning(w)
}, error = function(e) {
	stop(e)
}, finally = {
})
}
