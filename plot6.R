# plot6.R

# set working directory and environment
setwd("/Users/petrpodrouzek/Documents/coursera/DS_Exploratory_Data_Analysis_PROJECT2")
library(plyr)
library(ggplot2) 
library(reshape2)
library(grid)
library(gridExtra)

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
# approach I took is as follows: calculate relative changes but also attach absolute changes
# the relative changes can be calculated in two ways; it is either comparing all years to 1999 
# or comparing one period to the previous period (for example: comparing 2005 to 2002 etc.)
# outcome is then 3 rows of charts - 1. relative comparison to previous year, 2. relative comparison to Year 1999
# and finally 3. charts containing absolute values
aggPlot6 <- setNames(aggregate(NEI$Emissions, by = list(year = NEI$year, fips = NEI$fipsName), sum), c("Year", "Fips.Name", "Emissions"))
aggPlot6 <- merge(aggPlot6, aggPlot6[aggPlot6$Year == 1999, c("Emissions", "Fips.Name")], by = "Fips.Name")
aggPlot6 <- setNames(aggPlot6, c("Flips.Name", "Year", "Emissions", "Emissions.Year.1999"))
aggPlot6$Emissions.Previous.Year <- rbind(aggPlot6[1,],aggPlot6[1:3,],aggPlot6[5,],aggPlot6[5:7,])$Emissions 
aggPlot6$Ratio1 <- round((aggPlot6$Emissions - aggPlot6$Emissions.Previous.Year) / aggPlot6$Emissions.Previous.Year,2)
aggPlot6$Ratio2 <- round((aggPlot6$Emissions - aggPlot6$Emissions.Year.1999) / aggPlot6$Emissions.Year.1999,2)

# final data set with the ratios calculated
aggPlot6Summary <- melt(aggPlot6[c("Year", "Flips.Name", "Emissions" , "Ratio1", "Ratio2")], id=c("Year", "Flips.Name"))
aggPlot6Summary$variableName[aggPlot6Summary$variable == "Ratio1"] = "Compared To Prev. Y"
aggPlot6Summary$variableName[aggPlot6Summary$variable == "Ratio2"] = "Compared To Y 1999"
aggPlot6Summary$variableName[aggPlot6Summary$variable == "Emissions"] = ""
aggPlot6SummaryB <- aggPlot6Summary[aggPlot6Summary$variable == "Emissions",]
aggPlot6Summary <- aggPlot6Summary[aggPlot6Summary$variable != "Emissions",]

# plot the data using ggplot
plot6 <- (ggplot(aggPlot6Summary, aes(x=aggPlot6Summary$Year, y=aggPlot6Summary$value))
          + geom_line()
          + ggtitle("Change in PM25 Emissions by Year and Flips (Relative)") 
          + labs(x="Year",y="% Change")) 
plot6 <- plot6 + scale_x_discrete(name ="Year", limits=aggPlot6Summary$Year) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot6 <- plot6 + facet_grid(aggPlot6Summary$variableName ~ aggPlot6Summary$Flips.Name)

plot6b <- (ggplot(aggPlot6SummaryB, aes(x=aggPlot6SummaryB$Year, y=aggPlot6SummaryB$value))
          + geom_line()
          + ggtitle("Change in PM25 Emissions by Year and Flips (Absolute)") 
          + labs(x="Year",y="Emissions (tons)")) 
plot6b <- plot6b + scale_x_discrete(name ="Year", limits=aggPlot6SummaryB$Year) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot6b <- plot6b + facet_grid(aggPlot6SummaryB$variable ~ aggPlot6SummaryB$Flips.Name)

grid.arrange(plot6, plot6b)
g <- arrangeGrob(plot6, plot6b) 
ggsave("plot6.png", g)


