# plot1.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)

# load data as suggested in the problem defintion
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# start plotting
aggPlot1 <- setNames(aggregate(NEI$Emissions, by = list(year = NEI$year), sum), c("Year", "Emissions"))
minPlot1Y <- round_any(min(aggPlot1$Emissions), 1000000, floor)
maxPlot1Y <- round_any(max(aggPlot1$Emissions), 1000000, ceiling)
seqPlot1Y <- seq(minPlot1Y, maxPlot1Y, length.out = 3)

plot(aggPlot1, type="b", axes = F, ylim=c(minPlot1Y, maxPlot1Y), main = "Total PM25 Emissions", ylab = "Emissions (tons)")
axis(side=1, at=aggPlot1$Year)
axis(side=2, at=seqPlot1Y)
box()
text(aggPlot1$Year[1:3], aggPlot1$Emissions[1:3]+100000, cex = 0.7, labels=round(aggPlot1$Emissions[1:3]), pos = 4, offset = 0.25)
text(aggPlot1$Year[4], aggPlot1$Emissions[4]-100000, cex = 0.7, labels=round(aggPlot1$Emissions[4]), pos = 2, offset = 0.25)

# save plot to the file
dev.copy(png, 
         "plot1.png",
         height=480,
         width=480)
dev.off()

