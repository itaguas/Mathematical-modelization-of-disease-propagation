library(ggplot2)

setwd ("D:/Users/Nacho/Desktop/TFM/conectivity/network")

data <- read.csv("components_g.txt", header = T, sep = ";")

data$i <- NULL

head(data)

probs <- unique(data$prob)

mean <- c()

for (i in probs) {
  mean <- c(mean, mean(subset(data, data$prob == i)$comp))
}

data <- data.frame(probs, mean)

data <- subset(data, data$probs <= 0.4)

plot <- ggplot(data, aes(x = probs, y = mean)) +
  geom_line(size = 2, color = "blue") +
  theme_minimal() +
  labs(title = paste0(""),
       x = "R",
       y = "Number of nodes in the main component") +
  theme(plot.title = element_text (hjust = 0.5, size = 28, face = "bold"),
        axis.title.x = element_text (size = 45, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 45, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25)) +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_x_continuous(breaks = seq(0, 0.4, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5))


mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/network/geom_network.jpg")
jpeg(mypath, width = 1100, height = 1100)
  print(plot)
dev.off()
