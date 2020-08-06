library(ggplot2); library(magrittr); library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot 6 ------------------------------------------------------------------

motor <- as.vector(SCC[grep("[Mm]otor", SCC$Short.Name), ]$SCC)

NEI %>% 
  filter(SCC %in% motor, fips %in% c('24510', '06037')) %>% 
  group_by(year, fips) %>% 
  summarise(TotalEmissions = sum(Emissions)) %>% 
  ggplot(mapping = aes(x = year, y = TotalEmissions, col = fips)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  ylab("Total Emissions") +
  xlab("Year") +
  guides(color = guide_legend("Area")) + 
  labs(title = "Emissions of PM2.5 from Motor Vehicle Sources") + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  scale_color_manual(labels = c("Los Angeles", "Baltimore City"), 
                     values = c("red", "blue"))

dev.copy(png, "plot6.png")
dev.off()
