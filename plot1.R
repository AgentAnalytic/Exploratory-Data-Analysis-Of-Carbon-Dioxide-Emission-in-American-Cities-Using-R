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



#Plot 1:
###################################################################
new_data_grouped <- group_by(new_data,year)

#data_mean <- summarise_each(new_data_grouped,funs = "mean")

new_data_grouped$year <- as.factor(as.character(new_data_grouped$year)) 


png()


setwd("C:/Users/Portal X/Documents/Data Science Coursera/data/Emission")

png(filename = "plot1.png",width = 1600,height = 1600,res = 120)

par(mfrow = c(2,1))

plot(new_data_grouped$year,new_data_grouped$Emissions, main = "Decline in Emission from 1999-2008 from all Sources",xlab = "Years",ylab = "Emission(In Tons)",col=brewer.pal(4,"Spectral"))

model <- lm(year~Emissions,data = new_data )

abline(model,lty = 6 , col = "red")

mtext("The Slope of Red linear regression line is = -6.533314e-05 , a negative slope indicated decrease in Emission from 1999-2008")



data_mean <- summarise_each(new_data_grouped,funs = "mean")

barplot(data_mean$Emissions,data_mean$year, main = "Decline in Mean of Emission from 1999-2008",xlab = "Years",ylab = "Mean Of Emission(In Tons)", names = data_mean$year,col=brewer.pal(4,"Spectral"))




dev.off()

