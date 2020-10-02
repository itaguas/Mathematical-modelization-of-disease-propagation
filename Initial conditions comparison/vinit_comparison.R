library(ggplot2)
library(tidyr)

#Set working directory
setwd ("D:/Users/Nacho/Desktop/TFM/conectivity/vinit_comparison")

#Load the data, and check whether the format is correct
data1 <- read.csv("base_0.75.txt", header = T, sep = ";")
data1 <- as.data.frame(data1)
colnames(data1)

data2 <- read.csv("base_r.txt", header = T, sep = ";")
data2 <- as.data.frame(data2)
lapply(data2, class)

data2 <- subset(data2, data2$dn == 0.75)

bvinit <- data.frame(data1$prob, data1$healthy, data1$infected, data1$dead, data1$c_init, data1$c_final)
colnames(bvinit) <- c("prob", "healthy", "infected", "dead", "c_init", "c_final")
svinit <- data.frame(data2$prob, data2$healthy, data2$infected, data2$dead, data2$c_init, data2$c_final)
colnames(svinit) <- c("prob", "healthy", "infected", "dead", "c_init", "c_final")
svinit$vinit <- "k/2"
bvinit$vinit <- "k"
data <- rbind(svinit, bvinit)
prob_upper <- round(sort(data$prob, decreasing = TRUE)[1], 3) #Highest connectivity
prob_lower <- round(sort(data$prob)[1], 3) #Lowest connectivity

n_nodes <- data[1,]$healthy + data[1,]$dead + data[1,]$infected #Number of nodes

probs <- unique(data$prob)
vinits <- unique(data$vinit)

qsh1_upper <- c()
qsh1_lower <- c()
qsi1_upper <- c()
qsi1_lower <- c()
qsd1_upper <- c()
qsd1_lower <- c()

qsh2_upper <- c()
qsh2_lower <- c()
qsi2_upper <- c()
qsi2_lower <- c()
qsd2_upper <- c()
qsd2_lower <- c()

qstc_lower_k <- c()
qstc_upper_k <- c()
'qstc_lower_k/2' <- c()
'qstc_upper_k/2' <- c()

qsnc_lower_k <- c()
qsnc_upper_k <- c()
'qsnc_lower_k/2' <- c()
'qsnc_upper_k/2' <- c()

state_dn <- data.frame("P" = numeric(),
                       "Healthy" = numeric(),
                       "Infeted" = numeric(),
                       "Dead" = numeric(),
                       "Vinit" = character())

fconc_dn <- data.frame("P" = numeric(),
                       "t_Concentration" = numeric(),
                       "n_Concentration" = numeric(),
                       "Vinit" = character())

for (probel in probs){
  for (vinitel in vinits) {
    subs <- subset(data, data$prob == probel & data$vinit == vinitel)
    
    cur <- data.frame("P" = probel,
                      "Healthy" = mean(subs$healthy),
                      "Infected" = mean(subs$infected),
                      "Dead" = mean(subs$dead),
                      "Vinit" = vinitel)
    
    state_dn <- rbind(state_dn, cur)
    
    assign(paste0("qsh", match(vinitel, vinits), "_upper"),
           c(get(paste0("qsh", match(vinitel, vinits), "_upper")), 
             quantile(subs$healthy, 0.9)))
    assign(paste0("qsh", match(vinitel, vinits), "_lower"),
           c(get(paste0("qsh", match(vinitel, vinits), "_lower")), 
             quantile(subs$healthy, 0.1)))
    assign(paste0("qsi", match(vinitel, vinits), "_upper"),
           c(get(paste0("qsi", match(vinitel, vinits), "_upper")), 
             quantile(subs$infected, 0.9)))
    assign(paste0("qsi", match(vinitel, vinits), "_lower"),
           c(get(paste0("qsi", match(vinitel, vinits), "_lower")), 
             quantile(subs$infected, 0.1)))
    assign(paste0("qsd", match(vinitel, vinits), "_upper"),
           c(get(paste0("qsd", match(vinitel, vinits), "_upper")), 
             quantile(subs$dead, 0.9)))
    assign(paste0("qsd", match(vinitel, vinits), "_lower"),
           c(get(paste0("qsd", match(vinitel, vinits), "_lower")), 
             quantile(subs$dead, 0.1)))
    
    tot <- c()
    
    for (i in 1:nrow(subs)) {
      if (subs$infected[i] == 0) {
        tot <- c(tot, 0)
      }
      else {
        tot <- c(tot, subs$c_final[i]/(subs$infected[i]))
      }
    }
    
    fconc <- data.frame("P" = probel,
                        "t_Concentration" = mean(subs$c_final),
                        "n_Concentration" = mean(tot),
                        "Vinit" = vinitel)
    
    
    assign(paste0("qstc_lower_", vinitel),
           c(get(paste0("qstc_lower_", vinitel)), 
             quantile(subs$c_final, 0.1)))
    assign(paste0("qstc_upper_", vinitel),
           c(get(paste0("qstc_upper_", vinitel)), 
             quantile(subs$c_final, 0.9)))
    assign(paste0("qsnc_lower_", vinitel),
           c(get(paste0("qsnc_lower_", vinitel)), 
             quantile(tot, 0.1)))
    assign(paste0("qsnc_upper_", vinitel),
           c(get(paste0("qsnc_upper_", vinitel)), 
             quantile(tot, 0.9)))
    
    fconc_dn <- rbind(fconc_dn, fconc)
  }
}

state_g_dn <- gather(state_dn, value = "number", key = "State", -c(P, Vinit))

state_g_dn$Vinit <- factor(state_g_dn$Vinit, levels = c("k/2", "k"))

h1rib <- data.frame(probs, qsh1_upper, qsh1_lower)
colnames(h1rib) <- c("P", "upper", "lower")
i1rib <- data.frame(probs, qsi1_upper, qsi1_lower)
colnames(i1rib) <- c("P", "upper", "lower")
d1rib <- data.frame(probs, qsd1_upper, qsd1_lower)
colnames(d1rib) <- c("P", "upper", "lower")
h2rib <- data.frame(probs, qsh2_upper, qsh2_lower)
colnames(h2rib) <- c("P", "upper", "lower")
i2rib <- data.frame(probs, qsi2_upper, qsi2_lower)
colnames(i2rib) <- c("P", "upper", "lower")
d2rib <- data.frame(probs, qsd2_upper, qsd2_lower)
colnames(d2rib) <- c("P", "upper", "lower")

levels(state_g_dn$Vinit) <- c(levels(state_g_dn$Vinit), "0.375", "0.75")

state_g_dn$Vinit[state_g_dn$Vinit == "k"] <- "0.75"
state_g_dn$Vinit[state_g_dn$Vinit == "k/2"] <- "0.375"

state_plot <- ggplot(state_g_dn, aes(x = P, y = number, group = State, colour = Vinit)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("aquamarine3", "darkorange2")) +
  geom_ribbon(data = h1rib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "aquamarine3") +
  geom_ribbon(data = i1rib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "aquamarine3") +
  geom_ribbon(data = d1rib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "aquamarine3") +
  geom_ribbon(data = h2rib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "darkorange2") +
  geom_ribbon(data = i2rib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "darkorange2") +
  geom_ribbon(data = d2rib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "darkorange2") +
  theme_minimal() +
  labs(title = paste0(""),
       x = "Probability",
       y = "Number of nodes") +
  theme(plot.title = element_text (hjust = 0.5, size = 45, face = "bold"),
        axis.title.x = element_text (size = 45, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 45, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25)) +
  labs(colour="Initial pathogen\nconcentration",linetype="Initial pathogen\nconcentration",shape="Initial pathogen\nconcentration")  +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = round(seq(0, n_nodes, by = n_nodes/10), 3)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.016)) +
  coord_cartesian(ylim = c(0, n_nodes), xlim = c(prob_lower, prob_upper))
  
mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/vinit_comparison/state_comparison.jpg")
jpeg(mypath, width = 1000, height = 1000)
  print(state_plot)
dev.off()

fconc_dn$Vinit <- factor(fconc_dn$Vinit, levels = c("k/2", "k"))

n_conc_uppery <- max(qsnc_upper_k, get('qsnc_upper_k/2'))

qsnc_k <- data.frame(probs, qsnc_lower_k, qsnc_upper_k)
colnames(qsnc_k) <- c("P", "lower", "upper")
qsnc_k2 <- data.frame(probs, get('qsnc_lower_k/2'), get('qsnc_upper_k/2'))
colnames(qsnc_k2) <- c("P", "lower", "upper")
qstc_k <- data.frame(probs, qstc_lower_k, qstc_upper_k)
colnames(qstc_k) <- c("P", "lower", "upper")
qstc_k2 <- data.frame(probs, get('qstc_lower_k/2'), get('qstc_upper_k/2'))
colnames(qstc_k2) <- c("P", "lower", "upper")

n_conc_plot <- ggplot(fconc_dn, aes(x = P, y = n_Concentration, color = Vinit)) +
  geom_ribbon(data = qsnc_k, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "tomato4") +
  geom_ribbon(data = qsnc_k2, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "green4") +
  geom_line(size = 2) +
  scale_color_manual(values=c("tomato4", "green4")) +
  theme_minimal() +
  labs(title = "",
       x = "Probability",
       y = "Concentration") +
  theme(plot.title = element_text (hjust = 0.5, size = 28, face = "bold"),
        axis.title.x = element_text (size = 45, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 45, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25)) +
  #scale_y_continuous(labels = scales::scientific, breaks = round(seq(0, n_conc_uppery, by = n_conc_uppery/20), 5)) +
  labs(colour="Initial pathogen\nconcentration",linetype="Initial pathogen\nconcentration",shape="Initial pathogen\nconcentration")  +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.016)) +
  coord_cartesian(ylim = c(0, n_conc_uppery), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/vinit_comparison/node_concentration_comparison.jpg")
jpeg(mypath, width = 1000, height = 1000)
  print(n_conc_plot)
dev.off()



t_conc_uppery <- max(qstc_upper_k, get('qstc_upper_k/2'))

levels(fconc_dn$Vinit) <- c(levels(fconc_dn$Vinit), "0.375", "0.75")

fconc_dn$Vinit[fconc_dn$Vinit == "k"] <- "0.75"
fconc_dn$Vinit[fconc_dn$Vinit == "k/2"] <- "0.375"


t_conc_plot <- ggplot(fconc_dn, aes(x = P, y = t_Concentration, color = Vinit)) +
  geom_ribbon(data = qstc_k, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "aquamarine3") +
  geom_ribbon(data = qstc_k2, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "darkorange2") +
  geom_line(size = 2) +
  scale_color_manual(values = c("aquamarine3", "darkorange2")) +
  theme_minimal() +
  labs(title = "",
       x = "Probability",
       y = "Concentration") +
  theme(plot.title = element_text (hjust = 0.5, size = 28, face = "bold"),
        axis.title.x = element_text (size = 45, face = "bold", vjust = 0),
        axis.title.y = element_text (size = 45, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black", size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25)) +
  labs(colour="Initial pathogen\nconcentration",linetype="Initial pathogen\nconcentration",shape="Initial pathogen\nconcentration")  +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.016)) +
  coord_cartesian(ylim = c(0, 45), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/vinit_comparison/total_concentration_comparison.jpg")
jpeg(mypath, width = 1000, height = 1000)
  print(t_conc_plot)
dev.off()

