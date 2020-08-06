library(ggplot2); library(magrittr); library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 5 ------------------------------------------------------------------

motor <- as.vector(SCC[grep("[Mm]otor", SCC$Short.Name), ]$SCC)

NEI %>% 
  filter(SCC %in% motor, fips == '24510') %>% 
  group_by(year) %>% 
  summarise(TotalEmissions = sum(Emissions)) %$%
  plot(year, TotalEmissions, 
       type = "b", 
       col = "red", 
       ylab = "Total Emissions",
       xlab = "Year",
       main = "Emissions of PM2.5 from Motor Vehicle Sources in Baltimore City")

dev.copy(png, "plot5.png")
dev.off()
