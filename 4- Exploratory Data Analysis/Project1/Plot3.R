library(tidyverse)
library(lubridate)

# READING DATA
dat <- read_delim("power.txt", ";", skip = 66000, n_max = 5000, col_names = F)

# COMBINE DATE AND TIME
dat$dt <- with(dat, dmy(X1, tz = "GMT") + hms(X2))

# FILTER DATES
dat <- dat %>% filter(dt >= ymd_hms("2007-02-01 00:00:00"), 
                      dt <= ymd_hms("2007-02-02 23:59:00"))

# ENERGY SUB METERING PLOT 3
with(dat, {
    
plot(dt, X7, t = "l", xlab = "", ylab = "Energy Sub Metering", cex.lab = .8)
lines(dat$dt, dat$X8, type = "l", col = "red")
lines(dat$dt, dat$X9, t = "l", col = "blue")
legend("topright", lty = 1, col = c("black","blue", "red"),
       leg = c("Sub Metering 1", "Sub Metering 2", "Sub Metering 3"), cex = .8)
    
})
