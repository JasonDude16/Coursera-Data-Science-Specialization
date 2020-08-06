library(tidyverse)
library(lubridate)

# READING DATA
dat <- read_delim("power.txt", ";", skip = 66000, n_max = 5000, col_names = F)

# COMBINE DATE AND TIME
dat$dt <- with(dat, dmy(X1, tz = "GMT") + hms(X2))

# FILTER DATES
dat <- dat %>% filter(dt >= ymd_hms("2007-02-01 00:00:00"), 
                      dt <= ymd_hms("2007-02-02 23:59:00"))

# HISTOGRAM PLOT 1
hist(dat$X3, col = "red", main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)", cex.main = 0.9, cex.lab = 0.8)
