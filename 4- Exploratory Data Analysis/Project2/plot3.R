library(ggplot2); library(magrittr); library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

# Plot 3 ------------------------------------------------------------------

NEI %>% 
  filter(fips == "24510") %>% 
  group_by(year, type) %>% 
  summarise(TotalEmissions = sum(Emissions)) %>% 
  ggplot(mapping = aes(x = year, y = TotalEmissions)) +
  geom_line(color = "red") +
  geom_point() +
  theme_bw() + 
  facet_wrap(vars(type)) +
  labs(title = "Change in Total PM2.5 Emissions in Baltimore City by Type")

dev.copy(png, "plot3.png")
dev.off()
