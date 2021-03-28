install.packages("dplyr")
install.packages("ggpubr")
install.packages("car")
install.packages("ARTool")

library(dplyr)
library(ggpubr)
library(car)
library(ARTool)

###########################################################################################
###This part is for one factor
csv_data <- read.csv("data/anx_growth.csv", sep = ";", header = T)

ggdensity(csv_data$growth, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")
ggqqplot(csv_data$growth)

# Normality test
shapiro.test(csv_data$growth)
#From the output, the p-value > 0.05 implying that the distribution of the data
#are not significantly different from normal distribution.
#In other words, we can assume the normality.

# Homogeneity of Variance Test
res <- bartlett.test(growth ~ group, data = csv_data)
res
#From the output, it can be seen that the p-value of 0.7292 is not less than the
#significance level of 0.05. This means that there is no evidence to suggest that
#the variance in plant growth is statistically significantly different for the three treatment groups.


###########################################################################################
###This part is for two factors
csv_data <- read.csv("data/anx_other.csv", sep = ";", header = T)
table_cut <- subset(csv_data, sorting =="Carb_Prot") # condition (change for each tested condition)
table_cut <- table_cut [-c(1)]

table(table_cut$temperature, table_cut$condition)

# ggboxplot(table_cut, x = "temperature", y = "value", color = "condition",
#           palette = c("#00AFBB", "#E7B800"))
# 
# ggline(table_cut, x = "temperature", y = "value", color = "condition",
#        add = c("mean_se", "dotplot"),
#        palette = c("#00AFBB", "#E7B800"))

# Normality test
shapiro.test(table_cut$value)
# Homogeneity of Variance Test
leveneTest(value~
             as.factor(temperature)*as.factor(condition),data=table_cut)
#The p value is greater than 0.05, so equal variances can be assumed.


###########################################################################################
#ANOVA or Aligned Rank Transform

#ANOVA if data is normally distributed
res.aov2 <- aov(value ~ temperature + condition, data = table_cut)
summary(res.aov2)
#The above fitted model is called additive model.
#It makes an assumption that the two factor variables are independent.
#If you think that these two variables might interact to create an synergistic effect,
#replace the plus symbol (+) by an asterisk (*).

#ART if data is not normally distributed
m <- art(value ~ factor(temperature)*factor(condition), data=table_cut)
summary(m)
anova(m)

###########################################################################################
#Post hoc tests

#Test after Anova
TukeyHSD(res.aov2)


#Test after ART
#Transformations (choose one that will fit your data [ start with the first one]):
# Square-root  for moderate skew (positively skewed data):
test_norm <- sqrt(table_cut$value)
# Square-root  for moderate skew (negatively skewed data):
test_norm <- sqrt(max(table_cut$value + 1) - table_cut$value)
# Log for greater skew (positively skewed data):
test_norm <- log10(table_cut$value)
# Log for greater skew (negatively skewed data):
test_norm <- log10(max(table_cut$value + 1) - table_cut$value)
# Inverse for severe skew (positively skewed data):
test_norm <- 1/(table_cut$value)
# Inverse for severe skew (negatively skewed data):
test_norm <- 1/(max(table_cut$value + 1) - table_cut$value)

#Normality test
shapiro.test(test_norm)
#If the p-value > 0.05 we can assume normal distribution.

#Here you can check how data looks like:
ggdensity(test_norm)

# Now assign the "test_norm" group as "velue" column in "table_cut"
table_cut$value <- test_norm

#Continue as usuall with two-way Anova (remember of using correctly + or *)
res.aov2 <- aov(value ~ temperature * condition, data = table_cut)
summary(res.aov2)

#Post-Hoc Tukey test
TukeyHSD(res.aov2)
