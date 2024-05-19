library(ggpubr)
library(ggplot2)

###EXPLORATORY DATA ANALYSIS

# 3. average sound pressure level
summary(Ref_DE$avSPL)
summary(Ref_DK$avSPL)

## visualization
#histogram 
ggplot() +
  geom_histogram(data = Ref_DE, aes(x = avSPL), bins = 30, fill = 'blue', alpha = 0.5) +
  geom_histogram(data = Ref_DK, aes(x = avSPL), bins = 30, fill = 'orange', alpha = 0.5) +
  labs(title = "Average Sound Pressure Level", x = "Average SPL", y = "Count") +
  theme_minimal()

#boxplot
Ref_DE$group <- 'Dataset DE'
Ref_DK$group <- 'Dataset DK'
Ref_both <- rbind(Ref_DE, Ref_DK)

ggplot(Ref_both, aes(x = group, y = avSPL, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Sound Pressure Level", x = "Group", y = "Average SPL") +
  theme_minimal()

#violin plot
ggplot(Ref_both, aes(x = group, y = avSPL, fill = group)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Average Sound Pressure Level", x = "Group", y = "Average SPL") +
  theme_minimal()

#density plot
ggplot() +
  geom_density(data = Ref_DE, aes(x = avSPL), fill = 'blue', alpha = 0.5) +
  geom_density(data = Ref_DK, aes(x = avSPL), fill = 'orange', alpha = 0.5) +
  labs(title = "Density Plot of Average Sound Pressure Level", x = "Average SPL", y = "Density") +
  theme_minimal()

#scatter plot with jitter
ggplot(Ref_both, aes(x = group, y = avSPL, color = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Scatter Plot of Average Sound Pressure Level", x = "Group", y = "Average SPL") +
  theme_minimal()

#faceted histogram
ggplot(Ref_both, aes(x = avSPL, fill = group)) +
  geom_histogram(alpha = 0.6, position = 'identity', bins = 30) +
  facet_wrap(~group) +
  labs(title = "Histogram of Average Sound Pressure Level", x = "Average SPL", y = "Count") +
  theme_minimal()
