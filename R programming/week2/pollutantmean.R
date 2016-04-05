sysname <- Sys.info()["sysname"]
env <- if (sysname == "Windows") Sys.getenv("USERPROFILE") else Sys.getenv("HOME")

setwd(paste0(env, "/Desarrollo/Coursera/data-science/R programming/week2"))

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate"

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## int the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result

	oneZero <- "0"
	douZero <- "00"
	csvExtension <- ".csv"

	csvFile <- ""
	totalPollutant <- c()

	for (idFile in id) {
		if (idFile >= 1 && idFile <= 9)
			csvFile <- paste0(douZero, idFile, csvExtension)
		else if (idFile >= 10 && idFile <= 99)
			csvFile <- paste0(oneZero, idFile, csvExtension)
		else
			csvFile <- paste0(idFile, csvExtension)

		data <- read.csv(paste(directory, csvFile, sep = "/"))

		# Option 1
		dataPollutant <- data[!is.na(data[, pollutant]), pollutant]
		#dataPollutant <- na.omit(data[, pollutant])
		# Option 2 with NA
		#dataPollutant <- data[[pollutant]]

		totalPollutant <- c(totalPollutant, dataPollutant)
	}

	# Option 1
	round(mean(totalPollutant), 3)
	# Option 2
	#mean(totalPollutant, na.rm = TRUE)
}

print(pollutantmean("specdata", "sulfate", 1:10))
print(pollutantmean("specdata", "nitrate", 70:72))
print(pollutantmean("specdata", "nitrate", 23))