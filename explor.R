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

model <- lm(year~Emissions,data = data_mean )

abline(model,lty = 4)



#outlier_values <- boxplot.stats(data_set$Emissions)$out


#############################################################################
#Plot 2:
###########################################################################

Maryland_data <- subset.data.frame(new_data,new_data$fips == "24510")


Maryland_data$year <- as.factor(as.character(Maryland_data$year))


Maryland_data <- group_by(Maryland_data,year)

Maryland_data_mean <- summarise_each(Maryland_data,funs = "mean")

par(mfrow = C(1,2))

plot(Maryland_data$year,Maryland_data$Emissions, ylim = c(0,5),col=brewer.pal(4,"Spectral"))

model2 <- lm(mean(Emissions)~year,data = Maryland_data )

abline(model2,lty = 3,col = "red")


barplot(Maryland_data_mean$year,Maryland_data_mean$Emissions,col=brewer.pal(4,"Spectral"))


barplot(Maryland_data_mean$Emissions,Maryland_data_mean$year,col=brewer.pal(4,"Spectral"))





#barplot(Maryland_data$Emissions,as.factor(Maryland_data$year), main = "Decline in Pollution over the Years",xlab = "Years",ylab = "Mean Of Emission(In Tons)")


#################################################################################################################
#PLOT 3
################################################################################################################

#p1 <-ggplot(Maryland_data,aes(x = year,y = Emissions))
#p1 + geom_boxplot() + facet_wrap(~type)

Maryland_data$year <- as.numeric(as.character(Maryland_data$year))
Maryland_data_type <- group_by(Maryland_data,year,type)

Maryland_data_type_mean <- summarise_each(Maryland_data_type,funs = 'mean') 

p1 <-ggplot(Maryland_data_type,aes(x = year,y = Emissions,fill = year))
p1 + geom_boxplot() + facet_wrap(~type)+ geom_smooth(method='lm',formula= Emissions ~year)

p1 <-ggplot(Maryland_data_type,aes(x = year,y = Emissions, color = type))+ geom_point() +geom_smooth(method="lm",lwd = 1,lty = 3, se=TRUE, color="blue") + ylim(0,200)
p1 + facet_wrap(~type) 


+ ylim(0,3)






