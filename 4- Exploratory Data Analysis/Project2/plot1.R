library(ggplot2); library(magrittr); library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

# Plot 1 ------------------------------------------------------------------

NEI %>% 
  group_by(year) %>% 
  summarise(TotalEmissions = sum(Emissions)) %$%
  plot(year, TotalEmissions, 
       type = "b", 
       col = "red", 
       ylab = "Total Emissions",
       xlab = "Year",
       main = "Change in Total PM2.5 Emissions from All Sources in the US")

dev.copy(png, "plot1.png")
dev.off()
