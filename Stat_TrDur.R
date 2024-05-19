library(ggpubr)
library(ggplot2)

###EXPLORATORY DATA ANALYSIS

# 4.Train Duration in micro seconds
summary(Ref_DE$TrDur_us)
summary(Ref_DK$TrDur_us)

## visualization
#histogram 
ggplot() +
  geom_histogram(data = Ref_DE, aes(x = TrDur_us), bins = 30, fill = 'blue', alpha = 0.5) +
  geom_histogram(data = Ref_DK, aes(x = TrDur_us), bins = 30, fill = 'orange', alpha = 0.5) +
  labs(title = "Train Duration Distribution", x = "Average Train Duration", y = "Count") +
  theme_minimal()

#boxplot
Ref_DE$group <- 'Dataset DE'
Ref_DK$group <- 'Dataset DK'
Ref_both <- rbind(Ref_DE, Ref_DK)

ggplot(Ref_both, aes(x = group, y = TrDur_us, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Train Duration", x = "Group", y = "Average Train Duration") +
  theme_minimal()

#violin plot
ggplot(Ref_both, aes(x = group, y = TrDur_us, fill = group)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Average Train Duration", x = "Group", y = "Average Train Duration") +
  theme_minimal()

#density plot
ggplot() +
  geom_density(data = Ref_DE, aes(x = TrDur_us), fill = 'blue', alpha = 0.5) +
  geom_density(data = Ref_DK, aes(x = TrDur_us), fill = 'orange', alpha = 0.5) +
  labs(title = "Density Plot of Average Train Duration", x = "Average Train Duration", y = "Density") +
  theme_minimal()

#scatter plot with jitter
ggplot(Ref_both, aes(x = group, y = TrDur_us, color = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Scatter Plot of Average Train Duration", x = "Group", y = "Average Train Duration") +
  theme_minimal()

#faceted histogram
ggplot(Ref_both, aes(x = TrDur_us, fill = group)) +
  geom_histogram(alpha = 0.6, position = 'identity', bins = 30) +
  facet_wrap(~group) +
  labs(title = "Histogram of Average Train Duration", x = "Average Train Duration", y = "Count") +
  theme_minimal()


### CHECK ASSUMPTIONS / REQUIREMENTS

#checking if data follows normal distribution (Shapiro-Wilk (max. 5000 samples),  Kolmogorov-Smirnov or Q-Q-Plots)
qqnorm(Ref_DE$TrDur_us)
qqline(Ref_DE$TrDur_us)
ks.test(Ref_DE$TrDur_us, "pnorm", mean = mean(Ref_DE$TrDur_us), sd = sd(Ref_DE$TrDur_us))
# no normal distribution, significant difference between sample and normal distribution (ks test)

qqnorm(Ref_DK$TrDur_us)
qqline(Ref_DK$TrDur_us)
ks.test(Ref_DK$TrDur_us, "pnorm", mean = mean(Ref_DK$TrDur_us), sd = sd(Ref_DK$TrDur_us))
#very small p-value (< 2.2e-16) suggests that the sample distribution significantly differs from the normal distribution

#checking for homogeneity of variances (Levene's Test)
install.packages("car")
library(car)
leveneTest(TrDur_us ~ as.factor(c(rep(1, nrow(Ref_DE)), rep(2, nrow(Ref_DK)))), data = data.frame(TrDur_us = c(Ref_DE$TrDur_us, Ref_DK$TrDur_us)))
#p-value (<2.2e-16) < 0.05 --> statistically significant difference in variances between the groups
#null hypothesis of homogeneity of variances rejected

### STATISTICAL TEST

t.test(Ref_DE$TrDur_us, Ref_DK$TrDur_us, var.equal = FALSE)
wilcox.test(Ref_DE$TrDur_us, Ref_DK$TrDur_us)
#p (<2.2e-16) < 0.05 --> there is a significant difference between the datasets for the criterion median frequency