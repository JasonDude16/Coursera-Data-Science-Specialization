library(tidyverse)
library(lubridate)

# READING DATA
dat <- read_delim("power.txt", ";", skip = 66000, n_max = 5000, col_names = F)

# COMBINE DATE AND TIME
dat$dt <- with(dat, dmy(X1, tz = "GMT") + hms(X2))

# FILTER DATES
dat <- dat %>% filter(dt >= ymd_hms("2007-02-01 00:00:00"), 
                      dt <= ymd_hms("2007-02-02 23:59:00"))

# GAP PLOT 2
plot(dat$dt, dat$X3, t = "l", xlab = "", 
     ylab = "Global Active Power (kilowatts)", cex.lab = 0.7)
