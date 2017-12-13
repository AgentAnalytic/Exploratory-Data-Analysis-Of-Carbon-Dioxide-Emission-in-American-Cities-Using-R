library(dplyr)
library(lattice)
library(ggplot2)
library(stringr)
library(outliers)
library(RColorBrewer)

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

#Merging the two above data frame :
NEI$SCC <- as.factor(NEI$SCC)

data_set <- merge(NEI,SCC, by ="SCC",all.x = TRUE)


new_data <-arrange(data_set,desc(Emissions))

#Plot 1:
###################################################################
new_data_grouped <- group_by(new_data,year)

data_mean <- summarise_each(new_data_grouped,funs = "mean")

barplot(data_mean$Emissions,data_mean$year, main = "Decline in Pollution over the Years",xlab = "Years",ylab = "Mean Of Emission(In Tons)", names = data_mean$year,col=brewer.pal(4,"Spectral"))

#model <- lm(Emissions~year,data = data_mean )

#bline(model,lty = 4)



#outlier_values <- boxplot.stats(data_set$Emissions)$out


#############################################################################
#Plot 2:
###########################################################################

Maryland_data <- subset.data.frame(new_data,new_data$fips == "24510")



Maryland_data <- arrange(Maryland_data, desc(Emissions))

barplot(Maryland_data$Emissions,as.factor(Maryland_data$year), main = "Decline in Pollution over the Years",xlab = "Years",ylab = "Mean Of Emission(In Tons)")




 




