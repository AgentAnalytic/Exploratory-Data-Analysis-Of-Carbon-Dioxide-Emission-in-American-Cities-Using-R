library(dplyr)
library(lattice)
library(ggplot2)
library(stringr)
library(outliers)
library(RColorBrewer)
library(plyr)

#downloading the file :


#setwd("C:/Users/abhishek.katyal/Documents/ds")

setwd("C:/Users/Portal X/Documents/Data Science Coursera/data")


url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"


if(!file.exists("C:/Users/Portal X/Documents/Data Science Coursera/data/pollution.zip")){
  
  download.file(url, destfile = "C:/Users/abhishek.katyal/Documents/ds/pollution.zip")
}

unzip("pollution.zip")


#loading the data into a R object :

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Merging the two above data frame :
NEI$SCC <- as.factor(NEI$SCC)

data_set <- merge(NEI,SCC, by ="SCC",all.x = TRUE)


new_data <- data_set





#Plot 2:
###########################################################################

Maryland_data <- filter(new_data,new_data$fips == "24510")



Maryland_data <- group_by(Maryland_data,year)

Maryland_data_mean <- summarise_each(Maryland_data,funs = "mean")



png()


setwd("C:/Users/Portal X/Documents/Data Science Coursera/data/Emission")

png(filename = "plot2.png",width = 1600,height = 1600,res = 120)


par(mfrow = c(2,1))

plot(Maryland_data$year,Maryland_data$Emissions,main = "Decline in Emission from 1999-2008 from all Sources for Baltimore City",xlab = "Years",ylab = "Emission(In Tons)")

#Maryland_data$year <- as.numeric(as.character(Maryland_data$year))

model <- lm(year~Emissions,data=Maryland_data )

abline(model, col="blue")

mtext("The Slope of Red linear regression line is = -5.904078e-03 ,a negative slope indicated decrease in Emission from 1999-2008")

barplot(Maryland_data_mean$Emissions,Maryland_data_mean$year, main = "Decline in Mean of Emission from 1999-2008 for Baltimore City",xlab = "Years",ylab = "Mean Of Emission(In Tons)", names = data_mean$year,col=brewer.pal(4,"Spectral"))



dev.off()



