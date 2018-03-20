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


#Plot 4
#############################################################################################################################################################



data_coal <- filter(data_set,grepl("coal",EI.Sector,ignore.case = TRUE))



model <- lm(data_coal$Emissions ~ data_coal$year)

slope <- round(coef(model)[2],3)

if(slope < 0){
  
  status <- "Decreased"
}
if(slope >0){
  
  status <- "Increased"
}


slope_status <- cbind.data.frame(slope,status)



png()


setwd("C:/Users/Portal X/Documents/Data Science Coursera/data/Emission")

png(filename = "plot4.png",width = 1800,height = 1200,res = 100)



p1 <-ggplot(data_coal,aes(x = year,y = Emissions, color = type))+ geom_point()+geom_smooth(method="lm",lwd = 1,lty = 5, se=TRUE, color="blue") 

p1 + ggtitle("Change in Emissions from Coal Combustion- related sources from 1999-2008 Across United States", subtitle = "Negative Slope of Linear Regeression line(in blue) indicates Decrease in Emission from 1999-2008 and/or vice versa") +  geom_label(data= slope_status, inherit.aes=FALSE, aes(y = 15000,x = 2000,label=paste("Slope of Linear Regression Line=",slope,",Emission Status:",status)))

dev.off()