# plot3.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)
library(ggplot2) 

# load data as suggested in the problem defintion
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# start plotting
# filter out values for Baltimore only
subsetPlot3 <- NEI[NEI$fips == "24510",]
aggPlot3 <- setNames(aggregate(subsetPlot3$Emissions, by = list(year = subsetPlot3$year, type = subsetPlot3$type), sum), c("Year", "Type" ,"Emissions"))

plot3 <- (ggplot(aggPlot3, aes(x=aggPlot3$Year, y=aggPlot3$Emissions)) 
          + geom_line() 
          + geom_point(shape=1) 
          + ggtitle("Total PM25 Emissions by Year and Type in Baltimore") 
          + labs(x="Year",y="Emissions (tons)")) 
plot3 <- plot3 + scale_x_discrete(name ="Year", limits=aggPlot3$Year) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot3 <- plot3 + facet_grid(. ~ aggPlot3$Type)
plot3

# save plot to the file
dev.copy(png, 
         "plot3.png",
         height=480,
         width=480)
dev.off()
