library(ggplot2); library(dplyr); library(plyr)
dat <- mtcars

# SYNOPSIS 

## VIEW DATA 
head(dat)
str(dat)

## RECODING
dat$am <- as.factor(mapvalues(dat$am, c(0,1), c("Automatic", "Manual")))
dat$vs <- as.factor(mapvalues(dat$vs, c(0,1), c("V-shaped", "Straight")))
dat$cyl <- as.factor(dat$cyl)

# DATA SUMMARY
summary(dat)
table(dat$am, as.factor(dat$vs))
table(dat$am, dat$cyl)


# T-TEST
t.test(manual$mpg,auto$mpg,alternative = "less")$p.value

# PLOTS
# -------------------------------------------------------------------------
plot(dat)
hist(dat$mpg, col = "lightblue", breaks = 5)
hist(dat$mpg, col = "lightblue", breaks = 10)

ggplot(dat) + 
  geom_boxplot(aes(x = as.factor(vs), y = mpg)) + 
  geom_point(aes(x = as.factor(vs), y = mpg, col = am))

ggplot(dat, aes(x = as.factor(vs), y = mpg, fill = am)) + 
  geom_boxplot() 

par(mfrow = c(1,3))
plot(dat$cyl, dat$mpg)
plot(dat$vs, dat$mpg)
plot(dat$am, dat$mpg)


# -------------------------------------------------------------------------
shapiro.test(dat$mpg)

# -------------------------------------------------------------------------
## VARIANCE
## SUMMARY
## RECODING
# IID?
# LEVERAGE
# INFLUENCE
# MODEL COMPARISON

hist(dat$mpg)
summary(as.factor(dat$cyl))
model <- lm(mpg ~ wt + am, mtcars)
summary(model)
plot(model)
dfbetas(model)
car::vif(model)
sqrt(car::vif(model)) #SD version

ggplot(dat, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() + 
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank())


# MODELING ----------------------------------------------------------------

mod1 <- lm(mpg ~ am + vs + wt, data = dat)
summary(mod1)
car::vif(mod1)
plot(mod1)
