sysname <- Sys.info()["sysname"]
env <- if (sysname == "Windows") Sys.getenv("USERPROFILE") else Sys.getenv("HOME")

setwd(paste0(env, "/Desarrollo/Coursera/data-science/R programming/week2"))

complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return a data frame of the form
	## id	nobs
	## 1	117
	## 2	1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases

	oneZero <- "0"
	douZero <- "00"
	csvExtension <- ".csv"

	csvFile <- ""
	complete <- data.frame(numeric(), numeric())

	for (j in seq_along(id)) {
		#print(j)
		#print(id[j])
		if (id[j] >= 1 && id[j] <= 9)
			csvFile <- paste0(douZero, id[j], csvExtension)
		else if (id[j] >= 10 && id[j] <= 99)
			csvFile <- paste0(oneZero, id[j], csvExtension)
		else
			csvFile <- paste0(id[j], csvExtension)

		data <- read.csv(paste(directory, csvFile, sep = "/"))
		nrow <- nrow(data[complete.cases(data),])

		#print(nrow)

		complete <- rbind(complete, c(id[j], nrow))
	}
	colnames(complete) <- c("id", "nobs")
	complete
}

print(complete("specdata", 1))
print(complete("specdata", c(2, 4, 8, 10, 12)))
print(complete("specdata", 30:25))
print(complete("specdata", 3))