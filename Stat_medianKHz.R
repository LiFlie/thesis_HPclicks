# H_0: There is no difference in the mean frequency/... between the german and 
# danish Harbour Porpoise clicks

## R MARKDOWN??? 

library(ggplot2)

###EXPLORATORY DATA ANALYSIS

# 1.median frequency of whole train
summary(Ref_DE_Aschau$medianKHz)
summary(Ref_DE_BoEck$medianKHz)
summary(Ref_DE_Langholz$medianKHz)

summary(Ref_DE$medianKHz)

summary(Ref_DK_6621$medianKHz)
summary(Ref_DK_6623$medianKHz)
summary(Ref_DK_6625$medianKHz)

summary(Ref_DK$medianKHz)

## visualization
#histogram 
ggplot() +
  geom_histogram(data = Ref_DE, aes(x = medianKHz), bins = 30, fill = 'blue', alpha = 0.5) +
  geom_histogram(data = Ref_DK, aes(x = medianKHz), bins = 30, fill = 'orange', alpha = 0.5) +
  labs(title = "Mean Frequency Distribution", x = "Mean Frequency", y = "Count") +
  theme_minimal()

#boxplot
Ref_DE$group <- 'Dataset DE'
Ref_DK$group <- 'Dataset DK'
Ref_both <- rbind(Ref_DE, Ref_DK)

ggplot(Ref_both, aes(x = group, y = medianKHz, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean Frequency", x = "Group", y = "Mean Frequency") +
  theme_minimal()

#violin plot
ggplot(Ref_both, aes(x = group, y = medianKHz, fill = group)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Mean Frequency", x = "Group", y = "Mean Frequency") +
  theme_minimal()

#density plot
ggplot() +
  geom_density(data = Ref_DE, aes(x = medianKHz), fill = 'blue', alpha = 0.5) +
  geom_density(data = Ref_DK, aes(x = medianKHz), fill = 'orange', alpha = 0.5) +
  labs(title = "Density Plot of Mean Frequency", x = "Mean Frequency", y = "Density") +
  theme_minimal()

#scatter plot with jitter
ggplot(Ref_both, aes(x = group, y = medianKHz, color = group)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Scatter Plot of Mean Frequency", x = "Group", y = "Mean Frequency") +
  theme_minimal()

#faceted histogram
ggplot(Ref_both, aes(x = medianKHz, fill = group)) +
  geom_histogram(alpha = 0.6, position = 'identity', bins = 30) +
  facet_wrap(~group) +
  labs(title = "Histogram of Mean Frequency", x = "Mean Frequency", y = "Count") +
  theme_minimal()

### CHECK ASSUMPTIONS / REQUIREMENTS

#checking if data follows normal distribution (Shapiro-Wilk,  Kolmogorov-Smirnov or Q-Q-Plots)
qqnorm(Ref_DE$medianKHz)
qqline(Ref_DE$medianKHz)
ks.test(Ref_DE$medianKHz, "pnorm", mean = mean(Ref_DE$medianKHz), sd = sd(Ref_DE$medianKHz))
# no normal distribution, significant difference between sample and normal distribution (ks test)

qqnorm(Ref_DK$medianKHz)
qqline(Ref_DK$medianKHz)
ks.test(Ref_DK$medianKHz, "pnorm", mean = mean(Ref_DK$medianKHz), sd = sd(Ref_DK$medianKHz))
#very small p-value (< 2.2e-16) suggests that the sample distribution significantly differs from the normal distribution

#checking for homogeneity of variances (Levene's Test)
install.packages("car")
library(car)
leveneTest(medianKHz ~ as.factor(c(rep(1, nrow(Ref_DE)), rep(2, nrow(Ref_DK)))), data = data.frame(medianKHz = c(Ref_DE$medianKHz, Ref_DK$medianKHz)))
#p-value (0.004727) < 0.05 --> statistically significant difference in variances between the groups
#null hypothesis of homogeneity of variances rejected

### STATISTICAL TEST

t.test(Ref_DE$medianKHz, Ref_DK$medianKHz, var.equal = FALSE)
wilcox.test(Ref_DE$medianKHz, Ref_DK$medianKHz)
#p (<2.2e-16) < 0.05 --> there is a significant difference between the datasets for the criterion median frequency

##PAIRWISE COMPARISON PLOTS

library(GGally)
