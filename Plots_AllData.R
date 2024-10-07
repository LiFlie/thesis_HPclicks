library(readxl)
library(ggplot2)
library(car)
library(psych)
library(patchwork)
library(dplyr)
library(cowplot)

Ref_DE_all <- read_excel("DE_ref_all_75thr.xlsx")
Ref_DK_all <- read_excel("DK_ref_all_seas_75thr.xlsx")

Ref_DE_Aschau <- subset(Ref_DE_all[Ref_DE_all$Std == "AS", ], select = -Season)
Ref_DE_Aschau$group <- 'Aschau'
Ref_DE_BoEck <- subset(Ref_DE_all[Ref_DE_all$Std == "BoEck", ], select = -Season) #only spring 23
Ref_DE_BoEck$group <- 'BoEck'
Ref_DE_Langholz <- subset(Ref_DE_all[Ref_DE_all$Std == "LA", ], select = -Season)
Ref_DE_Langholz$group <- 'Langholz'
Ref_DK_6621 <- Ref_DK_all[Ref_DK_all$Std == "Palle1", ]
Ref_DK_6621$group <- '6621'
Ref_DK_6623 <- Ref_DK_all[Ref_DK_all$Std == "Palle2", ]
Ref_DK_6623$group <- '6623'
Ref_DK_6625 <- Ref_DK_all[Ref_DK_all$Std == "Palle3", ]
Ref_DK_6625$group <- '6625'
Ref_DE_all <- rbind(Ref_DE_Aschau, Ref_DE_BoEck, Ref_DE_Langholz)
Ref_DK_all <- rbind(Ref_DK_6621, Ref_DK_6623, Ref_DK_6625)
Ref_both <- rbind(Ref_DE_all,Ref_DK_all)

## Correlation of Parameters

pairs.panels(Ref_both[, c("MinICI_us", "medianKHz", "NofClstrs", "AvPRF", "nActualClx")], 
             method = "spearman", # Use Spearman correlation
             hist.col = "blue",   # Color of histograms
             density = TRUE,      # Add density plots
             ellipses = TRUE)     # Add correlation ellipses

## Combined Plots, Comparing German vs. Denmark looking at differnet parameters and all reference stations

#Comparison of shortest Interclick interval
p1 <- ggplot(Ref_DE_all, aes(x = MinICI_us, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,50000) +
  labs(title = "Shortest ICI", x = "Shortest ICI", y = "Density") +
  theme(legend.position = "bottom")

p2 <- ggplot(Ref_DK_all, aes(x = MinICI_us, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,50000) +
  labs(title = "Shortest ICI", x = "Shortest ICI", y = "Density") +
  theme(legend.position = "bottom")

p3 <- ggplot(Ref_DE_all, aes(x = group, y = MinICI_us, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  ylim(0,200000) +
  labs(title = "Shortest ICI", x = "Group", y = "Shortest ICI") +
  theme(legend.position = "none")

p4 <- ggplot(Ref_DK_all, aes(x = group, y = MinICI_us, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  ylim(0,200000) +
  labs(title = "Shortest ICI", x = "Group", y = "Shortest ICI") +
  theme(legend.position = "none")

final_plot <- plot_grid(p1, p2, p3, p4, ncol = 2, rel_heights = c(1, 1, 1, 1, 0.1, 0.1))
#removed rows containing non-finite values --> where??
print(final_plot)


#comparison between number of actual clicks
p1 <- ggplot(Ref_DE_all, aes(x = nActualClx, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,100) +
  labs(title = "Number of Actual Clicks", x = "Number of Actual Clicks", y = "Density") +
  theme(legend.position = "bottom")

p2 <- ggplot(Ref_DK_all, aes(x = nActualClx, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,100) +
  labs(title = "Number of Actual Clicks", x = "Number of Actual Clicks", y = "Density") +
  theme(legend.position = "bottom")

p3 <- ggplot(Ref_DE_all, aes(x = group, y = nActualClx, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Number of Actual Clicks", x = "Group", y = "Number of Actual Clicks") +
  theme(legend.position = "none")

p4 <- ggplot(Ref_DK_all, aes(x = group, y = nActualClx, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Number of Actual Clicks", x = "Group", y = "Number of Actual Clicks") +
  theme(legend.position = "none")

final_plot <- plot_grid(p1, p2, p3, p4, ncol = 2, rel_heights = c(1, 1, 1, 1, 0.1, 0.1))

print(final_plot)

#comparison of median frequence of whole train
p1 <- ggplot(Ref_DE_all, aes(x = medianKHz, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "median Frequency of whole Train", x = "median KHz", y = "Density") +
  theme(legend.position = "bottom")

p2 <- ggplot(Ref_DK_all, aes(x = medianKHz, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "median Frequency of whole Train", x = "median KHz", y = "Density") +
  theme(legend.position = "bottom")

p3 <- ggplot(Ref_DE_all, aes(x = group, y = medianKHz, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "median Frequency of whole Train", x = "Group", y = "median KHz") +
  theme(legend.position = "none")

p4 <- ggplot(Ref_DK_all, aes(x = group, y = medianKHz, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "median Frequency of whole Train", x = "Group", y = "median KHz") +
  theme(legend.position = "none")

final_plot <- plot_grid(p1, p2, p3, p4, ncol = 2, rel_heights = c(1, 1, 1, 1, 0.1, 0.1))

print(final_plot)

#comparison of reciprocal of mean interclick intervals
p1 <- ggplot(Ref_DE_all, aes(x = AvPRF, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,150) +
  labs(title = "Reciprocal of mean Interclick intervals", x = "AvPRF", y = "Density") +
  theme(legend.position = "bottom")

p2 <- ggplot(Ref_DK_all, aes(x = AvPRF, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,150) +
  labs(title = "Reciprocal of mean Interclick intervals", x = "AvPRF", y = "Density") +
  theme(legend.position = "bottom")

p3 <- ggplot(Ref_DE_all, aes(x = group, y = AvPRF, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Reciprocal of mean Interclick intervals", x = "Group", y = "AvPRF") +
  theme(legend.position = "none")

p4 <- ggplot(Ref_DK_all, aes(x = group, y = AvPRF, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Reciprocal of mean Interclick intervals", x = "Group", y = "AvPRF") +
  theme(legend.position = "none")

final_plot <- plot_grid(p1, p2, p3, p4, ncol = 2, rel_heights = c(1, 1, 1, 1, 0.1, 0.1))

print(final_plot)

#comparison number of clusters
p1 <- ggplot(Ref_DE_all, aes(x = NofClstrs, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,60) +
  labs(title = "Number of train clicks with cluster", x = "NofClstrs", y = "Density") +
  theme(legend.position = "bottom")

p2 <- ggplot(Ref_DK_all, aes(x = NofClstrs, fill = group)) +
  geom_density(alpha = 0.5) +
  xlim(0,60) +
  labs(title = "Number of train clicks with cluster", x = "NofClstrs", y = "Density") +
  theme(legend.position = "bottom")

p3 <- ggplot(Ref_DE_all, aes(x = group, y = NofClstrs, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Number of train clicks with cluster", x = "Group", y = "NofClstrs") +
  theme(legend.position = "none")

p4 <- ggplot(Ref_DK_all, aes(x = group, y = NofClstrs, colour = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Number of train clicks with cluster", x = "Group", y = "NofClstrs") +
  theme(legend.position = "none")

final_plot <- plot_grid(p1, p2, p3, p4, ncol = 2, rel_heights = c(1, 1, 1, 1, 0.1, 0.1))

print(final_plot)
