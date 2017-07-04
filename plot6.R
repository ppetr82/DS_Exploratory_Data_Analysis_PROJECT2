# plot6.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)
library(ggplot2) 

# load data as suggested in the problem defintion
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# start plotting
# get the right SCCs
# it was not specified how to find motor vehicle related sources, so I have run a grep
# on EI.Sector column and asked for all SCCs containing Vehicles
SCCPlot6 <- SCC[grep("(?i).*Vehicles.*", SCC$EI.Sector),c("SCC", "EI.Sector")]
NEI <- NEI[NEI$fips == "24510" | NEI$fips == "06037",]
NEI$fipsName <- ifelse(NEI$fips == "24510", "Baltimore City", "Los Angeles County")
NEI <- merge(NEI, SCCPlot6, by = "SCC")

# it is not specified how the change should be monitored
# approach I took is as follows
# everything will be relative change as there are significant differences between
# emissions between Baltimore City and Los Angeles County -> everything is in %
# Two changes calculated - once a change in % where value of emissions in 1999 is the 100%
# and once change observation over observation change (so for example 1999 -> 2002 etc.)
# in both cases Baltimore City is the more volatile
aggPlot6 <- setNames(aggregate(NEI$Emissions, by = list(year = NEI$year, fips = NEI$fipsName), sum), c("Year", "Fips.Name", "Emissions"))
aggPlot6 <- merge(aggPlot6, aggPlot6[aggPlot6$Year == 1999, c("Emissions", "Fips.Name")], by = "Fips.Name")
aggPlot6 <- setNames(aggPlot6, c("Flips.Name", "Year", "Emissions", "Emissions.Year.1999"))
aggPlot6$Emissions.Previous.Year <- rbind(aggPlot6[1,],aggPlot6[1:3,],aggPlot6[5,],aggPlot6[5:7,])$Emissions 
aggPlot6$Ratio1 <- round((aggPlot6$Emissions - aggPlot6$Emissions.Previous.Year) / aggPlot6$Emissions.Previous.Year,2)
aggPlot6$Ratio2 <- round((aggPlot6$Emissions - aggPlot6$Emissions.Year.1999) / aggPlot6$Emissions.Year.1999,2)

# final data set with the ratios calculated
aggPlot6Summary <- melt(aggPlot6[c("Year", "Flips.Name", "Ratio1", "Ratio2")], id=c("Year", "Flips.Name"))
aggPlot6Summary$variableText <- ifelse(aggPlot6Summary$variable == "Ratio1", "Comparison To Previous Period", "Comparison To Year 1999") 

# plot the data using ggplot
plot6 <- (ggplot(aggPlot6Summary, aes(x=aggPlot6Summary$Year, y=aggPlot6Summary$value))
          + geom_line()
          + ggtitle("Change in PM25 Emissions by Year and Flips") 
          + labs(x="Year",y="Emissions (tons)")) 
plot6 <- plot6 + scale_x_discrete(name ="Year", limits=aggPlot6Summary$Year) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot6 <- plot6 + facet_grid(aggPlot6Summary$variableText ~ aggPlot6Summary$Flips.Name)
plot6

# save plot to the file
ggsave("plot6.png")


