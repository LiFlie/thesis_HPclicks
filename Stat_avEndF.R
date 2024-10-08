library(ggpubr)
library(ggplot2)

###EXPLORATORY DATA ANALYSIS

# 2.average frequency of last cycle
summary(Ref_DE$avEndF)
summary(Ref_DK$avEndF)

## visualization
#histogram 
ggplot() +
  geom_histogram(data = Ref_DE, aes(x = avEndF), bins = 30, fill = 'blue', alpha = 0.5) +
  geom_histogram(data = Ref_DK, aes(x = avEndF), bins = 30, fill = 'orange', alpha = 0.5) +
  labs(title = "Average Frequency of last Cycle", x = "Average Frequency", y = "Count") +
  theme_minimal()

#boxplot
Ref_DE$group <- 'Dataset DE'
Ref_DK$group <- 'Dataset DK'
Ref_both <- rbind(Ref_DE, Ref_DK)

ggplot(Ref_both, aes(x = group, y = avEndF, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Frequency of last Cycle", x = "Group", y = "Average Frequency") +
  theme_minimal()

#violin plot
ggplot(Ref_both, aes(x = group, y = avEndF, fill = group)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of of Average Frequency of last Cycle", x = "Group", y = "Average Frequency") +
  theme_minimal()

#density plot
ggplot() +
  geom_density(data = Ref_DE, aes(x = avEndF), fill = 'blue', alpha = 0.5) +
  geom_density(data = Ref_DK, aes(x = avEndF), fill = 'orange', alpha = 0.5) +
  labs(title = "Density Plot of Average Frequency of last Cycle", x = "Average Frequency", y = "Density") +
  theme_minimal()

#scatter plot with jitter
ggplot(Ref_both, aes(x = group, y = avEndF, color = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Scatter Plot of Average Frequency of last Cycle", x = "Group", y = "Average Frequency") +
  theme_minimal()

#faceted histogram
ggplot(Ref_both, aes(x = avEndF, fill = group)) +
  geom_histogram(alpha = 0.6, position = 'identity', bins = 30) +
  facet_wrap(~group) +
  labs(title = "Histogram of Average Frequency of last Cycle", x = "Average Frequency", y = "Count") +
  theme_minimal()

### CHECK ASSUMPTIONS / REQUIREMENTS

#checking if data follows normal distribution (Shapiro-Wilk (max. 5000 samples),  Kolmogorov-Smirnov or Q-Q-Plots)
qqnorm(Ref_DE$avEndF)
qqline(Ref_DE$avEndF)
ks.test(Ref_DE$avEndF, "pnorm", mean = mean(Ref_DE$avEndF), sd = sd(Ref_DE$avEndF))
# no normal distribution, significant difference between sample and normal distribution (ks test)

qqnorm(Ref_DK$avEndF)
qqline(Ref_DK$avEndF)
ks.test(Ref_DK$avEndF, "pnorm", mean = mean(Ref_DK$avEndF), sd = sd(Ref_DK$avEndF))
#very small p-value (< 2.2e-16) suggests that the sample distribution significantly differs from the normal distribution

#checking for homogeneity of variances (Levene's Test)
install.packages("car")
library(car)
leveneTest(avEndF ~ as.factor(c(rep(1, nrow(Ref_DE)), rep(2, nrow(Ref_DK)))), data = data.frame(avEndF = c(Ref_DE$avEndF, Ref_DK$avEndF)))
#p-value (<2.2e-16) < 0.05 --> statistically significant difference in variances between the groups
#null hypothesis of homogeneity of variances rejected

### STATISTICAL TEST

t.test(Ref_DE$avEndF, Ref_DK$avEndF, var.equal = FALSE)
wilcox.test(Ref_DE$avEndF, Ref_DK$avEndF)
#p (<2.2e-16) < 0.05 --> there is a significant difference between the datasets for the criterion median frequency