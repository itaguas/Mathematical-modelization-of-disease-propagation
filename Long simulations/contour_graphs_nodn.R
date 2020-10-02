#library(finalfit)
library(ggplot2)
library(tidyr)

#Script that takes a file with the following header:
#prob;i;dn;init_comp;final_comp;init_deg;init_cnt;final_deg;final_cnt;healthy;infected;dead;c_init;c_final
  #Prob is the connectivity probability
  #i is the repetition number
  #init_comp is the initial number of components
  #final_comp is the final number of components
  #init_deg and init_cnt are the different degree values and the number of nodes with such values, respectively, at the beginning
  #final_deg and final_cnt are the different degree values and the number of nodes with such values, respectively, at the end
  #healthy, dead and infected are the number of nodes on each state
  #c_init and c_final are the initial and final pathogen concentrations, respectively

#Set working directory
setwd ("D:/Users/Nacho/Desktop/TFM/conectivity/long_simulations/base/nodn")

#Load the data, and check whether the format is correct
data <- read.csv("nodn_r.txt", header = T, sep = ";")
data <- as.data.frame(data)
colnames(data)
lapply(data, class) #All columns are in its correct format
#ff_glimpse(data) #Data overview
#data <- subset(data, data$dn == 75)


#Set graph conditions
prob_upper <- round(sort(data$prob, decreasing = TRUE)[1], 3) #Highest connectivity
prob_lower <- round(sort(data$prob)[1], 3) #Lowest connectivity
n_nodes <- data[1,]$healthy + data[1,]$dead + data[1,]$infected #Number of nodes


###############################
#for (i in seq(1, nrow(data))) {
  #if (data[i,]$c_init == data[i,]$c_final && data[i,]$infected == 1) {
  #  print(i)
  #  data[i,]$c_final = 0.75
  #}
  #else {
  #  print ("BIEEE")
 # }
#}
###############################


#Take the different dn and prob values
probs <- unique(data$prob)


#Dataset for the different concentrations
fconc_dn <- data.frame("P" = numeric(),
                       "t_Concentration" = numeric(),
                       "n_Concentration" = numeric())

state_dn <- data.frame("P" = numeric(),
                       "Healthy" = numeric(),
                       "Infeted" = numeric(),
                       "Dead" = numeric())

qsh_upper <- c()
qsh_lower <- c()
qsi_upper <- c()
qsi_lower <- c()
qsd_upper <- c()
qsd_lower <- c()

qsnc_upper <- c()
qsnc_lower <- c()
qstc_upper <- c()
qstc_lower <- c()

for (probel in probs) {
  
  assign(paste0("data_", probel),
         subset(data, data$prob == probel,
                select = -prob))
  data_prob <- get(paste0("data_", probel))
  
  current_state <- data.frame("P" = probel,
                              "Healthy" = mean(data_prob$healthy),
                              "Infected" = mean(data_prob$infected),
                              "Dead" = mean(data_prob$dead))
  
  qsh_upper <- c(qsh_upper, quantile(data_prob$healthy, 0.9))
  qsh_lower <- c(qsh_lower, quantile(data_prob$healthy, 0.1))
  qsi_upper <- c(qsi_upper, quantile(data_prob$infected, 0.9))
  qsi_lower <- c(qsi_lower, quantile(data_prob$infected, 0.1))
  qsd_upper <- c(qsd_upper, quantile(data_prob$dead, 0.9))
  qsd_lower <- c(qsd_lower, quantile(data_prob$dead, 0.1))
  
  state_dn <- rbind(state_dn, current_state)
  
  tot <- c()
  
  for (i in 1:nrow(data_prob)) {
    if (data_prob$infected[i] == 0) {
      tot <- c(tot, 0)
    }
    else {
      tot <- c(tot, data_prob$c_final[i]/(data_prob$infected[i]))
    }
  }
  
  
  fconc <- data.frame("P" = probel,
                      "t_Concentration" = mean(data_prob$c_final),
                      "n_Concentration" = mean(tot))
  
  qstc_lower <- c(qstc_lower, quantile(data_prob$c_final, 0.1))
  qstc_upper <- c(qstc_upper, quantile(data_prob$c_final, 0.9))
  qsnc_lower <- c(qsnc_lower, quantile(tot, 0.1))
  qsnc_upper <- c(qsnc_upper, quantile(tot, 0.9))
  
  fconc_dn <- rbind(fconc_dn, fconc)
  
}

state_g_dn <- gather(state_dn, value = "number", key = "State", - P)

hrib <- data.frame(probs, qsh_upper, qsh_lower)
colnames(hrib) <- c("P", "upper", "lower")
irib <- data.frame(probs, qsi_upper, qsi_lower)
colnames(irib) <- c("P", "upper", "lower")
drib <- data.frame(probs, qsd_upper, qsd_lower)
colnames(drib) <- c("P", "upper", "lower")

state_g_dn$State <- factor(state_g_dn$State, levels = c("Healthy", "Infected", "Dead"))
state_g_dn <- subset(state_g_dn, state_g_dn$State != "Dead")

state_plot<- ggplot(state_g_dn, aes(x = P, y = number, color = State)) +
  geom_ribbon(data = hrib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "forestgreen") +
  geom_ribbon(data = irib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "firebrick") +
#  geom_ribbon(data = drib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "black") +
  geom_line(size = 2) +
  theme_minimal() +
  scale_color_manual(values=c("forestgreen", "firebrick")) +#, "black")) +
  labs(title = "",
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
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = round(seq(0, n_nodes, by = n_nodes/10), 3)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.016)) +
  coord_cartesian(ylim = c(0, n_nodes), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/long_simulations/base/nodn/nodn_r.jpg")
jpeg(mypath, width = 1000, height = 1000)
  state_plot
dev.off()


n_conc_uppery <- max(qsnc_upper)

n_conc_plot <- ggplot(fconc_dn, aes(x = P, y = n_Concentration)) +
  geom_ribbon(aes(x = P, ymin = qsnc_lower, ymax = qsnc_upper), alpha = 0.4, inherit.aes = F) +
  geom_segment(aes(x = 0.026, xend = 0.026, y = 0, yend = 0.75), linetype="dashed", color = "red", size = 2) +
  geom_line(size = 2, color = "blue") +
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
  theme(legend.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25)) +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  #scale_y_continuous(labels = scales::scientific, breaks = round(seq(0, n_conc_uppery, by = n_conc_uppery/20), 5)) +
  scale_y_continuous(breaks = round(seq(0, n_conc_uppery, by = n_conc_uppery/20), 3)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.016)) +
  coord_cartesian(ylim = c(0, n_conc_uppery), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/long_simulations/base/nodn/nodn_r_node_concentration.jpg")
jpeg(mypath, width = 1000, height = 1000)
  n_conc_plot
dev.off()

t_conc_uppery <- max(qstc_upper)

t_conc_plot <- ggplot(fconc_dn, aes(x = P, y = t_Concentration)) +
  geom_ribbon(aes(x = P, ymin = qstc_lower, ymax = qstc_upper), alpha = 0.4, inherit.aes = F) +
  geom_line(size = 2, color = "blue") +
  theme_minimal() +
  labs(title = "",
    x = "Probability",
    y = "Concentration") +
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
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = seq(0, 120, by = 5)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.016)) +
  coord_cartesian(ylim = c(0, 120), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/TFM/conectivity/long_simulations/base/nodn/nodn_r_total_concentration.jpg")
jpeg(mypath, width = 1000, height = 1000)
  t_conc_plot
dev.off()
