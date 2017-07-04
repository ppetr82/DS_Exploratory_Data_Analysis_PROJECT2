# plot5.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)

# load data as suggested in the problem defintion
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# start plotting
# get the right SCCs
# it was not specified how to find motor vehicle related sources, so I have run a grep
# on EI.Sector column and asked for all SCCs containing Vehicles
SCCPlot5 <- SCC[grep("(?i).*Vehicles.*", SCC$EI.Sector),c("SCC", "EI.Sector")]
NEI <- NEI[NEI$fips == "24510",]
NEI <- merge(NEI, SCCPlot5, by = "SCC")
aggPlot5 <- setNames(aggregate(NEI$Emissions, by = list(year = NEI$year), sum), c("Year", "Emissions"))
minPlot5Y <- round_any(min(aggPlot5$Emissions), 100, floor)
maxPlot5Y <- round_any(max(aggPlot5$Emissions), 100, ceiling)
seqPlot5Y <- seq(minPlot5Y, maxPlot5Y, length.out = 3)

plot(aggPlot5, type="b", axes = F, ylim=c(minPlot5Y, maxPlot5Y), main = "Total Motor Vehicles Related PM25 Emissions \nin Baltimore", ylab = "Emissions (tons)")
axis(side=1, at=aggPlot5$Year)
axis(side=2, at=seqPlot5Y)
box()
text(aggPlot5$Year[1:3], aggPlot5$Emissions[1:3]+10, cex = 0.7, labels=round(aggPlot5$Emissions[1:3]), pos = 4, offset = 0.25)
text(aggPlot5$Year[4], aggPlot5$Emissions[4]-10, cex = 0.7, labels=round(aggPlot5$Emissions[4]), pos = 2, offset = 0.25)

# save plot to the file
dev.copy(png, 
         "plot5.png",
         height=480,
         width=480)
dev.off()

