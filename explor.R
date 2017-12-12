#downloading the file :


setwd("C:/Users/abhishek.katyal/Documents/ds")

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"


  if(!file.exists("C:/Users/abhishek.katyal/Documents/ds/pollution.zip")){

download.file(url, destfile = "C:/Users/abhishek.katyal/Documents/ds/pollution.zip")
}

unzip("pollution.zip")


#loading the data into a R object :

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

