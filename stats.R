#library(tidyverse)
#library(dplyr)
#library(ggplot2)
#library(summarytools)

# create df

# normal distribution with 2 groups
df <- data.frame(nums=rnorm(90),type=rep(c("a","b"), times=45))
df$row <- row(df)[,1]
df$nums_type <- as.integer(factor(df$type))

################################################################################

# independent samples t-test

t_tst <- function(xx,yy){
  val <- ((mean(xx) - mean(yy))/sqrt((sd(xx)^2/length(xx))+(sd(yy)^2/length(yy))))
  
  return (val)
}

#t-test 
df_x <- df[df$type=="a",]
df_x <- df_x$nums
df_y <- df[df$type=="b",]
df_y <- df_y$nums

t_tst <- t_tst(df_x,df_y) # custom function
t.test(nums~type, data=df) # or use built-in function

# sig and p-value mean what

# p <= 0.05 = accept h1 reject h0

# h0 is there is no difference between the types
# h1 is there is a significant difference between the types

### correlation test

cor1 <- cor.test(formula = ~ nums+nums_type, data=df)
cor1 <- cor.test(x=df$nums, y=df$nums_type)

# sig and p-value mean what

# p <= 0.05 = accept h1 reject h0

# h0 is there is no difference between the types
# h1 is there is a significant difference between the types

################################################################################

##### chi-square test

# one-sample, based on nums_type (2 possibilities)
chi1 <- chisq.test(x=table(df$type)) # category
chi1 <- chisq.test(x=table(df$nums_type)) # or number as category

# sig and p-value mean what

# p <= 0.05 = accept h1 reject h0

# h0 is there is no difference between the types
# h1 is there is a significant difference between the types

# 2-sample
chi2 <- chisq.test(x=table(df$type,df$nums_type))

# sig and p-value mean what

################################################################################

##### anova
# one-way anova
# https://bookdown.org/ndphillips/YaRrr/ex-one-way-anova.html
aov1 <- aov(formula = nums ~ factor(type), data=df)

# view summary
aov_sum <- summary(aov1)

# post-hoc tests, Tukey HSD
aov_tky <- TukeyHSD(aov1)

# p <= 0.05 = accept h1 reject h0

# h0 is there is no difference between the types
# h1 is there is a significant difference between the types

# lm - same formula as anova to look at coefficients
lm1 <- lm(formula = nums ~ factor(type), data=df)
lm1_sum <- summary(lm1)

# two-way anova


################################################################################

# mann-whitney u test
# test whether there is a difference in the dependent variable in two independent groups

# in this case testing the type "a" or "b" against the nums (values) within each group

mwu <- wilcox.test(df$nums~factor(df$type))

# print the median and interquartile ranges
med_df <- data.frame(tapply(df$nums,df$type,median,na.rm=T))
iqr_df <- data.frame(tapply(df$nums,df$type,IQR,na.rm=T))
print(paste("mwu",med_df, iqr_df))

# if p_value >= 0.05 no significant difference

# h0 is there is no difference between the types
# h1 is there is a significant difference between the types

################################################################################

##### linear model
# https://bookdown.org/ndphillips/YaRrr/the-linear-model.html

# make linear model
lm1 <- lm(nums~row, data=df)

# view summary of model
lm1_sum <- summary(lm1)

# see details of coefficients
lm1_sum_co <- summary(lm1)$coefficients

# add fitted values back to df
df$lm_fits <- lm1$fitted.values

# plot the predicted values against the true values
plot(x = df$nums,                          # True values on x-axis
     y = df$lm_fits,               # fitted values on y-axis
     xlab = "True Values",
     ylab = "Model Fitted Values",
     main = "Regression fits of diamond values")

abline(b = 1, a = 0)                             # Values should fall around this line!

# predict on new values
new_df <- data.frame(row=91:100)
new_preds <- predict(object=lm1, newdata=new_df)
new_df$preds <- new_preds

######## linear model with interactions
# if using interactions in model need to use mean adjusted values

# created centered versions
df$centered_row <- df$row - mean(df$row)

# create centered lm
lm1_c <- lm(nums~centered_row, data=df)

# summary of centered lm
summary(lm1_c)$coefficients

# ANOVA object from regression
lm_aov <- summary(aov(lm1))
lm_c_aov <- summary(aov(lm1_c))

####### regression on non normal data
# use generalised linear model glm

# binary flag
df$flag <- df$nums > 0.05 #ifelse(df$nums > 0.05,1,0)

# glm based on true (1) or false (0)
glm1 <- glm(formula = flag~row, data = df, family = "binomial") # family flag can be adjusted as needed

# access fitted values the same way
df$glm_fits <- glm1$fitted.values

# as logit transformations, to get percentage predictions need to inverse logit
new_df$glm_preds <- 1/predict(object=glm1,newdata=new_df) # percentage that value is greater than 0.05