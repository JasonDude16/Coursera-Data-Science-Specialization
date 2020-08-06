library(ggplot2); library(dplyr); library(tidyr)

setwd("~/Desktop/Code/Coursera/Data Science/CDSS")
setwd("./5- Reproducible Research/Project2/data")
data <- read.csv("repdata_data_StormData.csv",)

data$EVTYPE <- as.factor(data$EVTYPE)

# -------------------------------------------------------------------------

harmful_sum <- 
  data %>% 
    group_by(EVTYPE) %>% 
    summarise(EVTYPE_count = n(),
              Deaths = sum(FATALITIES),
              Injuries = sum(INJURIES),
              Total = Deaths + Injuries) %>% 
    arrange(desc(total)) %>% 
    pivot_longer(cols = c("Deaths", "Injuries"),
               names_to = c("Type"))

p1 <- 
  ggplot(harmful_sum[1:20,], aes(value, reorder(EVTYPE, value), fill = type)) +
    geom_col() +
    theme_bw() + 
    labs(title = "Most Harmful Weather Events by Total Deaths and Injuries") + 
    xlab("Count") +
    ylab("Event") + 
    scale_fill_brewer(palette = "Set2") + 
    theme(panel.grid.major.y = element_blank())

plot(p1)

# -------------------------------------------------------------------------

harmful_avg <- 
  data %>% 
    group_by(EVTYPE) %>% 
    summarise(EVTYPE_count = n(),
              Deaths = sum(FATALITIES) / EVTYPE_count,
              Injuries = sum(INJURIES) / EVTYPE_count,
              Total = Deaths + Injuries) %>% 
    arrange(desc(Total)) %>% 
    filter(EVTYPE_count > 50) %>% 
    pivot_longer(cols = c("Deaths", "Injuries"),
                 names_to = c("Type"))
  
p2 <-
  ggplot(harmful_avg[1:20,], aes(value, reorder(EVTYPE, value), fill = Type)) +
    geom_col() +
    theme_bw() + 
    labs(title = "Most Harmful Weather Events by Average Deaths and Injuries") + 
    xlab("Count") +
    ylab("Event") + 
    scale_fill_brewer(palette = "Set2") + 
    theme(panel.grid.major.y = element_blank())
  
plot(p2)

# -------------------------------------------------------------------------
  
gridExtra::grid.arrange(p1, p2)
  
# -------------------------------------------------------------------------
  
  summary(as.factor(data$PROPDMGEXP))
  summary(as.factor(data$CROPDMGEXP))
  summary(data$PROPDMG)
  summary(data$CROPDMG)
  is.numeric(data$PROPDMG)
  is.numeric(data$CROPDMG)

# -------------------------------------------------------------------------

eco_sum <-
    data %>% 
    group_by(EVTYPE) %>% 
    filter(as.factor(PROPDMGEXP) == "M", as.factor(CROPDMGEXP) == "M") %>% 
    summarise(EVTYPE_count = n(),
              `Property Damage` = sum(PROPDMG),
              `Crop Damage` = sum(CROPDMG),
              Total = `Property Damage` + `Crop Damage`) %>% 
    arrange(desc(Total)) %>% 
    pivot_longer(cols = c("Property Damage", "Crop Damage"),
                 names_to = c("Type"))
  
p3 <-
  ggplot(eco_sum[1:20,], aes(value, reorder(EVTYPE, value), fill = Type)) +
    geom_col() +
    theme_bw() + 
    labs(title = "Weather Events with Greatest Economic Consequence") + 
    xlab("Damage (millions of dollars)") +
    ylab("Event") + 
    scale_fill_brewer(palette = "Pastel1") + 
    theme(panel.grid.major.y = element_blank())
  
plot(p3)
  
# -------------------------------------------------------------------------

eco_avg <- 
    data %>% 
      group_by(EVTYPE) %>% 
      filter(as.factor(PROPDMGEXP) == "M", as.factor(CROPDMGEXP) == "M") %>% 
      summarise(EVTYPE_count = n(),
                `Property Damage` = sum(PROPDMG) / EVTYPE_count,
                `Crop Damage` = sum(CROPDMG) / EVTYPE_count,
                Total = `Property Damage` + `Crop Damage`) %>% 
      arrange(desc(Total)) %>% 
      pivot_longer(cols = c("Property Damage", "Crop Damage"),
                  names_to = c("Type"))
  
p4 <-
  ggplot(eco_avg[1:20,], aes(value, reorder(EVTYPE, value), fill = Type)) +
    geom_col() +
    theme_bw() + 
    labs(title = "Weather Events with Greatest Economic Consequence, on Average") + 
    xlab("Damage (millions of dollars)") +
    ylab("Event") + 
    scale_fill_brewer(palette = "Pastel1") + 
    theme(panel.grid.major.y = element_blank())
  
plot(p4)

# -------------------------------------------------------------------------

gridExtra::grid.arrange(p3, p4)

  