# plot4.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)

# load data as suggested in the problem defintion
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# start plotting
# get the right SCCs
# it was not specified how to find coal combustion related sources, so I have run a grep
# on EI.Sector column and asked for all SCCs containing Comb (combustion) and Coal
SCCPlot4 <- SCC[grep("(?i).*comb.*coal.*|.*coal.*comb.*", SCC$EI.Sector),c("SCC", "EI.Sector")]
NEI <- merge(NEI, SCCPlot4, by = "SCC")
aggPlot4 <- setNames(aggregate(NEI$Emissions, by = list(year = NEI$year), sum), c("Year", "Emissions"))
minPlot4Y <- round_any(min(aggPlot4$Emissions), 100000, floor)
maxPlot4Y <- round_any(max(aggPlot4$Emissions), 100000, ceiling)
seqPlot4Y <- seq(minPlot4Y, maxPlot4Y, length.out = 3)

plot(aggPlot4, type="b", axes = F, ylim=c(minPlot4Y, maxPlot4Y), main = "Total Coal Combustion Related PM25 Emissions", ylab = "Emissions (tons)")
axis(side=1, at=aggPlot4$Year)
axis(side=2, at=seqPlot4Y)
box()
text(aggPlot4$Year[1:3], aggPlot4$Emissions[1:3]+10000, cex = 0.7, labels=round(aggPlot4$Emissions[1:3]), pos = 4, offset = 0.25)
text(aggPlot4$Year[4], aggPlot4$Emissions[4]-10000, cex = 0.7, labels=round(aggPlot4$Emissions[4]), pos = 2, offset = 0.25)

# save plot to the file
dev.copy(png, 
         "plot4.png",
         height=480,
         width=480)
dev.off()
