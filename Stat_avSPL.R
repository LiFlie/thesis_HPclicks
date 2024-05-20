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

### CHECK ASSUMPTIONS / REQUIREMENTS

#checking if data follows normal distribution (Shapiro-Wilk (max. 5000 samples),  Kolmogorov-Smirnov or Q-Q-Plots)
qqnorm(Ref_DE$avSPL)
qqline(Ref_DE$avSPL)
ks.test(Ref_DE$avSPL, "pnorm", mean = mean(Ref_DE$avSPL), sd = sd(Ref_DE$avSPL))
# no normal distribution, significant difference between sample and normal distribution (ks test)

qqnorm(Ref_DK$avSPL)
qqline(Ref_DK$avSPL)
ks.test(Ref_DK$avSPL, "pnorm", mean = mean(Ref_DK$avSPL), sd = sd(Ref_DK$avSPL))
#very small p-value (< 2.2e-16) suggests that the sample distribution significantly differs from the normal distribution

#checking for homogeneity of variances (Levene's Test)
install.packages("car")
library(car)
leveneTest(avSPL ~ as.factor(c(rep(1, nrow(Ref_DE)), rep(2, nrow(Ref_DK)))), data = data.frame(avSPL = c(Ref_DE$avSPL, Ref_DK$avSPL)))
#p-value (<2.2e-16) < 0.05 --> statistically significant difference in variances between the groups
#null hypothesis of homogeneity of variances rejected

### STATISTICAL TEST

t.test(Ref_DE$avSPL, Ref_DK$avSPL, var.equal = FALSE)
wilcox.test(Ref_DE$avSPL, Ref_DK$avSPL)
#p (<2.2e-16) < 0.05 --> there is a significant difference between the datasets for the average sound pressure level