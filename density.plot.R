# ========================================#========================================

# density.plot w/ frequency bars and data rug / a reproducible example of advanced data visualization

# ========================================#========================================

# load libraries
library(dplyr)
library(ggplot2)

# ========================================#========================================

# set wd 
getwd()
setwd("/Users/amx/data_local/R/github/density.plot/") # change wd to your wd

# read data
items <- read.table(file = "density.plot.data.csv", header = TRUE, sep = ",", dec = ".", na.strings = "NA", stringsAsFactors = FALSE)
str(items)
View(items)

# mean over items
items <- dplyr::mutate(items, items.mean = (econ_04 + econ_05 + econ_06 + econ_07)/4)

# mean and sd
mean <- mean(items$items.mean, na.rm = TRUE) # mean of variable
sd <- sd(items$items.mean, na.rm = TRUE) # sd of variable

# ========================================#========================================

# print density plot
png(filename="density.plot.png", width=1024, height=1024*0.80, units="px")
par(mfrow=c(1,1))

dens <- qplot(x = items$items.mean, data = items, geom = "blank") +
  geom_histogram(aes(y=..density..), bins = 15, colour="grey", fill="grey") +
  geom_density(color = "darkturquoise", fill = "darkturquoise", alpha = .5, size=0.7) +
  
  # normal curve with mean and sd of insec
  stat_function(fun = dnorm, args = c(mean = mean, sd = sd), col = "red3", size=1) + 
  
  # vertical line = mean
  geom_vline(xintercept = mean, size = 2, colour = "black", linetype = "dashed") + 
  
  # data rug
  geom_rug(aes(x = items.mean, y = 0), color = "darkturquoise", position = position_jitter(width = 0.1,height = 0), alpha = 0.5) + 
  
  # theme = classic
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "grey90")) +

  # axis titles and limits
  labs(x = "Variable A", y = "Density") +
  theme(axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20)) +
  ylim(0,1) +
  xlim(1,5)
dens

dev.off()

# ========================================#========================================

