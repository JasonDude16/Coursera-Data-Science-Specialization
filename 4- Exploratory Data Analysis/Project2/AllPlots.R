library(ggplot2); library(magrittr); library(dplyr)

setwd("~/Desktop/Code/Coursera/Data Science/CDSS/4- Exploratory Data Analysis")
setwd("./Projects/Project2/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
sample <- NEI[round(runif(100000, 1, 6000000), 0), ]

# -------------------------------------------------------------------------

NEI %>% 
  group_by(year) %>% 
  summarise(TotalEmissions = sum(Emissions)) %$%
  plot(year, TotalEmissions, 
       type = "b", 
       col = "red", 
       ylab = "Total Emissions",
       xlab = "Year",
       main = "Change in Total PM2.5 Emissions from All Sources in the US")

# -------------------------------------------------------------------------

subset(NEI, fips == "24510") %>% 
  group_by(year) %>% 
  summarise(TotalEmissions = sum(Emissions)) %$%
  plot(year, TotalEmissions, 
       type = "b", 
       col = "red", 
       ylab = "Total Emissions",
       xlab = "Year",
       main = "Change in Total PM2.5 Emissions in Baltimore City")

# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------

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

