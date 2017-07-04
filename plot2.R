# plot2.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)

# load data as suggested in the problem defintion
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# start plotting
# filter out values for Baltimore only
subsetPlot2 <- NEI[NEI$fips == "24510",]
aggPlot2 <- setNames(aggregate(subsetPlot2$Emissions, by = list(year = subsetPlot2$year), sum), c("Year", "Emissions"))
minPlot2Y <- round_any(min(aggPlot2$Emissions), 1000, floor)
maxPlot2Y <- round_any(max(aggPlot2$Emissions), 1000, ceiling)
seqPlot2Y <- seq(minPlot2Y, maxPlot2Y, length.out = 3)

plot(aggPlot2, type="b", axes = F, ylim=c(minPlot2Y, maxPlot2Y) , main = "Total PM25 Emissions In Baltimore City", ylab = "Emissions (tons)")
axis(side=1, at=aggPlot2$Year)
axis(side=2, at=seqPlot2Y)
box()
text(aggPlot2$Year[c(1,3)], aggPlot2$Emissions[c(1,3)]+100, cex = 0.7, labels=round(aggPlot2$Emissions[c(1,3)]), pos = 4, offset = 0.25)
text(aggPlot2$Year[2], aggPlot2$Emissions[2]-100, cex = 0.7, labels=round(aggPlot2$Emissions[2]), pos = 2, offset = 0.25)
text(aggPlot2$Year[4], aggPlot2$Emissions[4]-100, cex = 0.7, labels=round(aggPlot2$Emissions[4]), pos = 2, offset = 0.25)

# save plot to the file
dev.copy(png, 
         "plot2.png",
         height=480,
         width=480)
dev.off()
