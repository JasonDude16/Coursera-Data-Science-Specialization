library(ggplot2); library(magrittr); library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 4 ------------------------------------------------------------------

coal_comb <- as.vector(SCC[grep("[Cc]oal", SCC$Short.Name), ]$SCC)

NEI %>% 
  filter(SCC %in% coal_comb) %>% 
  group_by(year) %>% 
  summarise(TotalEmissions = sum(Emissions)) %$%
  plot(year, TotalEmissions, 
       type = "b", 
       col = "red", 
       ylab = "Total Emissions",
       xlab = "Year",
       main = "Coal Combustion-related Sources of Emission of PM2.5 in the US")

dev.copy(png, "plot4.png")
dev.off()
