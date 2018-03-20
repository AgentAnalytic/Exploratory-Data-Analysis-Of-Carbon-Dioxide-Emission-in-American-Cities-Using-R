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


#PLOT 3
################################################################################################################


Maryland_data$year <- as.numeric(as.character(Maryland_data$year))
Maryland_data_type <- group_by(Maryland_data,year,type)

Maryland_data_type_model <- group_by(Maryland_data,type)


regression <- function(df) {
  reg_fun <- lm(formula = df$Emissions ~df$year)
  
  slope <-round(coef(reg_fun)[2], 2) 
  
  if (slope < 0) {emission_status <- "Decreased"}
  
  if (slope == 0) {emission_status <- "Unchanges"}
  
  if (slope > 0) {emission_status <- "Increased"}
  
  c(slope,emission_status)
}





regressions_data<- ddply(Maryland_data_type_model,"type",regression)
colnames(regressions_data)<- c("type","slope","status")
regressions_data$slope <- as.numeric(regressions_data$slope)


for(i in 1:4){
  
  if(regressions_data$slope[i] ==min(regressions_data$slope) & regressions_data$slope[i] <0){regressions_data$MinMax[i] <- "Maximum"}
  
  else if(regressions_data$slope[i] ==max(regressions_data$slope) & regressions_data$slope[i] <0){regressions_data$MinMax[i] <- "Minimum"}
  
  else if(regressions_data$slope[i] ==min(regressions_data$slope) & regressions_data$slope[i] >0){regressions_data$MinMax[i] <- "Minimim"}
  
  else if(regressions_data$slope[i] ==max(regressions_data$slope) & regressions_data$slope[i] >0){regressions_data$MinMax[i] <- "Maximum"}
  
  else{ regressions_data$MinMax[i] <- ""}
}



png()


setwd("C:/Users/Portal X/Documents/Data Science Coursera/data/Emission")

png(filename = "plot3.png",width = 1800,height = 1200,res = 100)



p1 <-ggplot(Maryland_data_type,aes(x = year,y = Emissions, color = type,label = Maryland_data_type$fips))+ geom_point()+geom_smooth(method="lm",lwd = 0.3,lty = 5, se=TRUE, color="blue") 

p1 + facet_wrap(~type)  +  geom_label(data= regressions_data, inherit.aes=FALSE, aes(x = 2004, y = 1000,label=paste("Slope of Linear Regression Line=",slope,",Emission Status for Source",type, ":",status,MinMax))) + ggtitle("Change in Emission of Baltimore City for different Source Types from 1999 to 2008",subtitle = "Negative Slope of Linear Regeression line(blue) indicates Decrease in Emission from 1999-2008 and/or vice versa")

dev.off()





