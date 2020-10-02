#As input, uses a files separated by ";" and with a header. The header of one file must contain:
###prob: the probability of a random network
###initial_infected: the number of infected nodes when the first death occurs
###last_infected: the number of infected nodes when the last death occurs
###death_interval: time interval between the first death and the last death



#As output, produces four graphs:
###death_intervals.jpg: shows the time interval between the first and last deaths
###infected.jpg: shows the number of infected nodes by the time the first node dies, and the number of infected nodes by the time the last nodes dies
###infected_difference.jpg: shows the difference between the number of infected nodes by the time the first node dies and the number of infected nodes by the time the last node dies
###infected_difference_regression.jpg: shows the same as infected_difference, but in this case using a regression line. This regression line is drawn using the gam (generalized additive mode) method, using a cubic regression spline



library(ggplot2)
library(tidyr)



#Set working directory
setwd ("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave")

#Load the data, and check whether the format is correct
data <- read.csv("death_interval.txt", header = T, sep = ";")
data <- as.data.frame(data)
colnames(data)

lapply(data, class) #All columns are in its correct format

data$prob <- as.character(data$prob) #Convert the probability columnt ot character, so that in can be used in boxplot graphs

#Graph that shows a boxplot for each probability with the time interval between the first and the last death
death_interval_plot <- ggplot(data = data, aes(x = prob, y = death_interval, group = prob)) +
  geom_boxplot(fill = "forestgreen", alpha = 0.25, outlier.color = NA) +
  geom_point(aes(fill = prob), position=position_jitterdodge()) + 
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.1) +
  theme_minimal() +
  labs(title = paste0(""),
       x = "Probability",
       y = "Time") +
  theme(plot.title = element_text (hjust = 0.5, size = 67, face = "bold"),
        axis.title.x = element_text (size = 67, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 67, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.title = element_text(size = 45),
        legend.text = element_text(size = 37.7),
        axis.text = element_text(size = 37.5)) +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(8, 50, by = 2)) +
  coord_cartesian(ylim = c(8, 50))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave/death_intervals.jpg")
jpeg(mypath, width = 3000, height = 1500)
  print(death_interval_plot)
dev.off()

#Generate dataframe with prob, and the initial and last infected
data2 <- data.frame(data$initial_infected, data$last_infected, data$prob)
colnames(data2) = c("First death", "Last death", "P")
#Gather the columns with initial and final infected
data2g <- gather(data2, value = "number", key = "Time", -P)
#Put the factors in the correct order
data2g$Time <- factor(data2g$Time, levels = c("First death", "Last death"))

#Graph that shows two boxplots for each probability: one for the number of infected nodes by the first death, and another for the number of infected nodes by the last death
infected_num_plot <- ggplot(data = data2g, aes(x = P, y = number, fill = Time)) +
  geom_boxplot(alpha = 0.25, outlier.colour = NA) +
  geom_point(aes(col=Time),position=position_jitterdodge()) + 
  theme_minimal() +
  labs(title = paste0("Number of infected by death wave"),
       x = "Probability",
       y = "Number of nodes") +
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
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = seq(0, 100, by = 4)) +
  coord_cartesian(ylim = c(0, 100))
  
mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave/infected.jpg")
jpeg(mypath, width = 1200, height = 1000)
  print(infected_num_plot)
dev.off()

#Create a column with the difference of infected nodes between the first and last death
data$lapse <- data$initial_infected - data$last_infected

#Graph with a boxplot for each probability with the difference between the number of infected nodes by the first death and the number of infected nodes by the last death
infected_dif_plot <- ggplot(data = data, aes(x = prob, y = lapse, group = prob)) +
  geom_boxplot(fill = "forestgreen", alpha = 0.25, outlier.color = NA) +
  geom_point(aes(fill = prob), position=position_jitterdodge()) + 
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.1) +
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
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 52, by = 2)) +
  coord_cartesian(ylim = c(0, 52))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave/infected_difference.jpg")
jpeg(mypath, width = 1200, height = 1000)
  print(infected_dif_plot)
dev.off()

#Reconvert the prob column to numeric, for the regression plot
data$prob <- as.numeric(data$prob)

#The same plot as the previous one, but this time as a regression line
regression_plot <- ggplot(data = data, aes(x = prob, y = lapse)) +
  geom_point() + 
  geom_smooth(color = "black", size = 2) +
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
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 52, by = 2)) +
  scale_x_continuous(breaks = seq(0.01, 0.1, by = 0.005)) +
  coord_cartesian(ylim = c(0, 32))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/death_wave/infected_difference_regression.jpg")
jpeg(mypath, width = 1200, height = 1000)
  print(regression_plot)
dev.off()