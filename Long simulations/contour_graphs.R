#library(finalfit)
library(ggplot2)
library(tidyr)

#Script that takes a file with the following header:
#prob;i;dn;init_comp;final_comp;init_deg;init_cnt;final_deg;final_cnt;healthy;infected;dead;c_init;c_final
  #Prob is the connectivity R
  #i is the repetition number
  #init_comp is the initial number of components
  #final_comp is the final number of components
  #init_deg and init_cnt are the different degree values and the number of nodes with such values, respectively, at the beginning
  #final_deg and final_cnt are the different degree values and the number of nodes with such values, respectively, at the end
  #healthy, dead and infected are the number of nodes on each state
  #c_init and c_final are the initial and final pathogen concentrations, respectively

#Set working directory
setwd ("D:/Users/Nacho/Desktop")

#Load the data, and check whether the format is correct
data <- read.csv("basef_g_1.txt", header = T, sep = ";")
data <- as.data.frame(data)
colnames(data)
lapply(data, class) #All columns are in its correct format
#ff_glimpse(data) #Data overview
data <- subset(data, data$dn != 75)


#Set graph conditions
prob_upper <- round(sort(data$prob, decreasing = TRUE)[1], 3) #Highest connectivity
prob_lower <- round(sort(data$prob)[1], 3) #Lowest connectivity
n_nodes <- data[1,]$healthy + data[1,]$dead + data[1,]$infected #Number of nodes


#Take the different dn and prob values
dns <- unique(data$dn)
probs <- unique(data$prob)



#Functions to get python strings
pylist_len <- function(pylist) {
  cha <- as.character(pylist)
  cha <- gsub("\\[", '', cha)
  cha <- gsub("\\(", '', cha)
  cha <- gsub("\\]", '', cha)
  cha <- gsub("\\)", '', cha)
  vector <- as.numeric(unlist(strsplit(cha, ",")))
  vector <- vector[!vector == 1]
  
  return(length(vector))
}



#Dataset for the different concentrations
fconc_dn <- data.frame("P" = numeric(),
                       "dn" = character(),
                       "t_Concentration" = numeric(),
                       "n_Concentration" = numeric())

comp_df <- data.frame("P" = numeric(), 
                      "dn" = character(), 
                      "Components" = numeric())


for (dnel in dns) {
  assign(paste0("data_", dnel),
         subset(data, data$dn == dnel,
                select = -dn))
  data_dn <- get(paste0("data_", dnel))
  
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
  
  assign(paste0("qsnc_upper_", dnel), c())
  assign(paste0("qsnc_lower_", dnel), c())
  assign(paste0("qstc_upper_", dnel), c())
  assign(paste0("qstc_lower_", dnel), c())
         
  for (probel in probs) {
    
    assign(paste0("data_", dnel, "_", probel),
           subset(data_dn, data_dn$prob == probel,
                  select = -prob))
    data_dn_prob <- get(paste0("data_", dnel, "_", probel))
    
    current_state <- data.frame("P" = probel,
                                "Healthy" = mean(data_dn_prob$healthy),
                                "Infected" = mean(data_dn_prob$infected),
                                "Dead" = mean(data_dn_prob$dead))
    
    qsh_upper <- c(qsh_upper, quantile(data_dn_prob$healthy, 0.9))
    qsh_lower <- c(qsh_lower, quantile(data_dn_prob$healthy, 0.1))
    qsi_upper <- c(qsi_upper, quantile(data_dn_prob$infected, 0.9))
    qsi_lower <- c(qsi_lower, quantile(data_dn_prob$infected, 0.1))
    qsd_upper <- c(qsd_upper, quantile(data_dn_prob$dead, 0.9))
    qsd_lower <- c(qsd_lower, quantile(data_dn_prob$dead, 0.1))
    
    state_dn <- rbind(state_dn, current_state)
    
    icomp <- c()
    fcomp <- c()
    
    tot <- c()
    
    for (i in 1:nrow(data_dn_prob)) {
      if (dnel == dns[1]){
        icomp <- c(icomp, pylist_len(data_dn_prob[i,2]))
      }
      fcomp <- c(fcomp, pylist_len(data_dn_prob[i, 3]))
      
      if (data_dn_prob$infected[i] == 0) {
        tot <- c(tot, 0)
      }
      else {
        tot <- c(tot, data_dn_prob$c_final[i]/(data_dn_prob$infected[i]))
      }
    }
    
    compon <- data.frame("P" = probel,
                        "dn" = toString(dnel),
                        "Components" = mean(fcomp))
    
    
    comp_df <- rbind(comp_df, compon)
    
    if (dnel == dns[1]) {
      compon <- data.frame("P" = probel,
                           "dn" = "ALL",
                           "Components" = mean(icomp))
      comp_df <- rbind(comp_df, compon)
    }
    
    
    fconc <- data.frame("P" = probel,
                        "dn" = toString(dnel),
                        "t_Concentration" = mean(data_dn_prob$c_final),
                        "n_Concentration" = mean(tot))
    
    assign(paste0("qstc_lower_", dnel),
           c(get(paste0("qstc_lower_", dnel)), 
             quantile(data_dn_prob$c_final, 0.1)))
    assign(paste0("qstc_upper_", dnel),
           c(get(paste0("qstc_upper_", dnel)), 
             quantile(data_dn_prob$c_final, 0.9)))
    assign(paste0("qsnc_lower_", dnel),
           c(get(paste0("qsnc_lower_", dnel)), 
             quantile(tot, 0.1)))
    assign(paste0("qsnc_upper_", dnel),
           c(get(paste0("qsnc_upper_", dnel)), 
             quantile(tot, 0.9)))
    
    fconc_dn <- rbind(fconc_dn, fconc)
    
  }
  
  state_g_dn <- gather(state_dn, value = "number", key = "State", - P)
  
  assign(paste0("state_", dnel),
         state_dn)
  
  assign(paste0("state_g_", dnel),
         state_g_dn)
  
  hrib <- data.frame(probs, qsh_upper, qsh_lower)
  colnames(hrib) <- c("P", "upper", "lower")
  irib <- data.frame(probs, qsi_upper, qsi_lower)
  colnames(irib) <- c("P", "upper", "lower")
  drib <- data.frame(probs, qsd_upper, qsd_lower)
  colnames(drib) <- c("P", "upper", "lower")
  
  state_g_dn$State <- factor(state_g_dn$State, levels = c("Healthy", "Infected", "Dead"))
  
  state_plot_dn <- ggplot(state_g_dn, aes(x = P, y = number, color = State)) +
    geom_ribbon(data = hrib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "forestgreen") +
    geom_ribbon(data = irib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "firebrick") +
    geom_ribbon(data = drib, aes(x = P, ymin = lower, ymax = upper), alpha = 0.4, inherit.aes = F, fill = "black") +
    geom_line(size = 2) +
    theme_minimal() +
    scale_color_manual(values=c("forestgreen", "firebrick", "black")) +
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
    theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
    scale_y_continuous(breaks = round(seq(0, n_nodes, by = n_nodes/10), 3)) +
    scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.04)) +
    coord_cartesian(ylim = c(0, n_nodes), xlim = c(prob_lower, prob_upper))
  
  assign(paste0("state_plot_", dnel),
         state_plot_dn)
}


n_conc_uppery <- max(qsnc_upper_0.75)
fconc_dn <- subset(fconc_dn, fconc_dn$dn != 0.675)

#colnames(fconc_dn) <- c("P", "Death threshold", "t_Concentration", "n_Concentration")

n_conc_plot <- ggplot(fconc_dn, aes(x = P, y = n_Concentration, color = dn)) +
  geom_ribbon(data = hrib, aes(x = P, ymin = qsnc_lower_0.375, ymax = qsnc_upper_0.375), alpha = 0.4, inherit.aes = F, fill = "aquamarine3") +
  #geom_ribbon(data = irib, aes(x = P, ymin = qsnc_lower_0.675, ymax = qsnc_upper_0.675), alpha = 0.4, inherit.aes = F, fill = "darkorange2") +
  geom_ribbon(data = hrib, aes(x = P, ymin = qsnc_lower_0.75, ymax = qsnc_upper_0.75), alpha = 0.4, inherit.aes = F, fill = "darkorchid4") +
  geom_line(size = 2) +
  scale_color_manual(values=c("aquamarine3", "darkorange2")) +#, "darkorchid4")) +
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
  theme(legend.title = element_text(size = 35),
        legend.text = element_text(size = 35),
        axis.text = element_text(size = 35)) +
  labs(colour="Death threshold",linetype="Death threshold",shape="Death threshold")  +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  #scale_y_continuous(labels = scales::scientific, breaks = round(seq(0, n_conc_uppery, by = n_conc_uppery/20), 5)) +
  scale_y_continuous(breaks = round(seq(0, n_conc_uppery, by = n_conc_uppery/20), 3)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.04)) +
  coord_cartesian(ylim = c(0, n_conc_uppery), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/geom/node_concentration.jpg")
jpeg(mypath, width = 1300, height = 1000)
  n_conc_plot
dev.off()

t_conc_uppery <- max(qstc_upper_0.75)

t_conc_plot <- ggplot(fconc_dn, aes(x = P, y = t_Concentration, color = dn)) +
  geom_ribbon(data = hrib, aes(x = P, ymin = qstc_lower_0.375, ymax = qstc_upper_0.375), alpha = 0.4, inherit.aes = F, fill = "aquamarine3") +
  #geom_ribbon(data = irib, aes(x = P, ymin = qstc_lower_0.675, ymax = qstc_upper_0.675), alpha = 0.4, inherit.aes = F, fill = "darkorchid4") +
  geom_ribbon(data = hrib, aes(x = P, ymin = qstc_lower_0.75, ymax = qstc_upper_0.75), alpha = 0.4, inherit.aes = F, fill = "darkorange2") +
  geom_line(size = 2) +
  scale_color_manual(values=c("aquamarine3", "darkorange2")) +#, "darkorchid4")) +
  theme_minimal() +
  labs(title = "",
    x = "R",
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
  labs(colour="Death\nthreshold",linetype="Death\nthreshold",shape="Death\nthreshold")  +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0))) +
  scale_y_continuous(breaks = seq(0, 55, by = 5)) +
  scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.04)) +
  coord_cartesian(ylim = c(0, 55), xlim = c(prob_lower, prob_upper))

mypath <- file.path("D:/Users/Nacho/Desktop/geom/total_concentration.jpg")
jpeg(mypath, width = 1000, height = 1000)
  t_conc_plot
dev.off()

mypath <- file.path("D:/Users/Nacho/Desktop/geom/0.375.jpg")
jpeg(mypath, width = 1000, height = 1000)
  state_plot_0.375
dev.off()

mypath <- file.path("D:/Users/Nacho/Desktop/geom/0.675.jpg")
jpeg(mypath, width = 1000, height = 1000)
  state_plot_0.675
dev.off()

mypath <- file.path("D:/Users/Nacho/Desktop/geom/0.75.jpg")
jpeg(mypath, width = 1000, height = 1000)
  state_plot_0.75
dev.off()


# comp_plot <- ggplot(comp_df, aes(x = P, y = Components, color = dn)) +
#   geom_line(size = 2) +
#   theme_minimal() +
#   labs(title = "Initial and final components by dn",
#        x = "Probability,
#        y = "Number of components") +
#   theme(plot.title = element_text (hjust = 0.5, size = 16, face = "bold"),
#         axis.title.x = element_text (size = 14, face = "bold", vjust = 0),
#         axis.title.y = element_text (size = 14, face = "bold", vjust = 1.5),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks = element_line(colour = "black", size=1),
#         panel.border = element_rect(colour = "black", fill=NA, size=1)) +
#   scale_y_continuous(breaks = seq(0, max(comp_df$Components), by = 1)) +
#   scale_x_continuous(breaks = seq(prob_lower, prob_upper, by = 0.006)) +
#   coord_cartesian(ylim = c(0, max(comp_df$Components)), xlim = c(prob_lower, prob_upper))
# comp_plot
