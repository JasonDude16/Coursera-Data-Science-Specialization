library(ggplot2); library(magrittr); library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

# Plot 2 ------------------------------------------------------------------

subset(NEI, fips == "24510") %>% 
  group_by(year) %>% 
  summarise(TotalEmissions = sum(Emissions)) %$%
  plot(year, TotalEmissions, 
       type = "b", 
       col = "red", 
       ylab = "Total Emissions",
       xlab = "Year",
       main = "Change in Total PM2.5 Emissions in Baltimore City")

dev.copy(png, "plot2.png")
dev.off()
