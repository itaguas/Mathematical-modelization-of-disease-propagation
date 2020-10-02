#As input, uses two files, separated by ";" and with a header

#The header of one file must contain:
###prob: the probability of a random network
###initial_infected: the number of infected nodes when the first death occurs
###last_infected: the number of infected nodes when the last death occurs

#The header of the other file must contain:
###prob: the probability of a random network
###dn: the death threshold
###dead: the total number of dead nodes



#As output, produces a graph that compares the regression lines generated from both files



library(ggplot2)
library(tidyr)



#Set working directory
setwd ("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave")

#Load the data, and check whether the format is correct
data1 <- read.csv("death_interval.txt", header = T, sep = ";")
data1 <- as.data.frame(data1)
colnames(data1)

#Create a column with the difference of infected nodes between the first and last death
data1$lapse <- data1$initial_infected - data1$last_infected

lapply(data1, class) #All columns are in its correct format

setwd ("D:/Users/Nacho/Desktop/TFM/conectivity/long_simulations")

data2 <- read.csv("base_r.txt", header = T, sep = ";")
data2 <- as.data.frame(data2)
lapply(data2, class)

#Extract the rows corresponding to a death threshold of 0'75
data2 <- subset(data2, data2$dn == 0.75)

#Graph that compares the regression lines obtained from the lapse and dead columns
regression_plot <- ggplot() +
  #geom_point() + 
  geom_smooth(data = data1, aes(x = prob, y = lapse, color = "steelblue"), size = 2, se = F) +
  geom_smooth(data = data2, aes(x = prob, y = dead,  color = "firebrick"), size = 2, se = F) +
  theme_minimal() +
  labs(title = paste0("Infected difference"),
       x = "Probability",
       y = "Node difference") +
  theme(plot.title = element_text (hjust = 0.5, size = 28, face = "bold"),
        axis.title.x = element_text (size = 24, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 24, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.title = element_text(size = 22),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20)) +
  scale_color_identity(name = "Curve",
                       labels = c("Dead", "Infected difference"),
                       guide = "legend") +
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  scale_x_continuous(breaks = seq(0.004, 0.1, by = 0.008)) +
  coord_cartesian(ylim = c(0, 30), xlim = c(0.004, 0.1))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave/curve_comparison.jpg")
jpeg(mypath, width = 1200, height = 1000)
  print(regression_plot)
dev.off()