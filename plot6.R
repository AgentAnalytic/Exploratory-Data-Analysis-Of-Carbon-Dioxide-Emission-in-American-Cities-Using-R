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




#Plot 6
############################################################################################################


data_compare <- filter(data_set,data_set$fips == "24510" |  data_set$fips == "06037")

data_compare <- filter(data_compare,grepl("Vehicle",data_compare$SCC.Level.Two))


data_compare_fips <- group_by(data_compare,fips)


data_compare_fips$fips <- gsub("06037","Los Angeles County, California",data_compare_fips$fips)

data_compare_fips$fips <- gsub("24510","Baltimore City, Maryland",data_compare_fips$fips)



regression <- function(df) {
  reg_fun <- lm(formula = df$Emissions ~df$year)
  
  slope <-round(coef(reg_fun)[2], 2) 
  
  if (slope < 0) {emission_status <- "Decreased"}
  
  if (slope == 0) {emission_status <- "Unchanges"}
  
  if (slope > 0) {emission_status <- "Increased"}
  
  c(slope,emission_status)
}


regressions_data<- ddply(data_compare_fips,"fips",regression)
colnames(regressions_data)<- c("fips","slope","status")
regressions_data$slope <- as.numeric(regressions_data$slope)


png()


setwd("C:/Users/Portal X/Documents/Data Science Coursera/data/Emission")

png(filename = "plot6.png",width = 1800,height = 1200,res = 100)





p1 <-ggplot(data_compare_fips,aes(x = year,y = Emissions, color = type))+ geom_point()+geom_smooth(method="lm",lwd = 0.3,lty = 5, se=TRUE, color="blue") 

p1 + facet_wrap(~fips)  +  geom_label(data= regressions_data, inherit.aes=FALSE, aes(x = 2004, y = 1600,label=paste("Slope of Liner Regression Line=",slope,",Emission Status:",status))) + ggtitle("Comparison of Emission from Motor Vehicles between Baltimore City & California from 1999-2008",subtitle = "Negative Slope of Linear Regeression indicates Decrease in Emission from 1999-2008 and/or vice versa (California(fips=06037) & Baltimore City(fips=24510))")

dev.off()

