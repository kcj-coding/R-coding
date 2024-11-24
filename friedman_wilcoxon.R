library(dplyr)
library(ggplot2)

# https://www.datanovia.com/en/lessons/friedman-test-in-r/

# create df
nums <- rep(rnorm(90),3)
labels <- rep(c("a","b","c"), 90)
xyz <- rep(c(1,2,3), each=30)

df <- data.frame(labels=labels, nums=nums, xyz=xyz)

# get means for friedman test
df_agg <- aggregate(nums ~ xyz+labels, df, mean)

# graph boxplots of the data
ggplot(df, aes(x=labels, y=nums))+
  geom_boxplot()

# friedman non-parametric test on these values
# test to check for statistically significant differences between distributions of paired groups
# p-value of < 0.05 implies distributions are different; > 0.05 implies no difference
friedman.test(nums ~ labels | xyz, df_agg)

# then Bonferroni test for significance
# p-value < 0.05 implies statistically significant differences; > 0.05 implies no significant difference
labels_fct <- factor(df_agg$labels)#, levels=c("a","b","c"), labels=c("a","b","c"))
pairwise.wilcox.test(df_agg$nums, labels_fct, p.adj = "bonf")

# then Kolmogorov–Smirnov test to see if two sample values are from same sample distribution
# a p-value of < 0.05 implies two samples drawn from different data distributions; > 0.05 implies same data distributions
ks.test(df_agg$nums[df_agg$labels=="b"], df_agg$nums[df_agg$labels=="c"])

# Kruskal–Wallis anova test on these values (non-parametric test)
# p-value < 0.05 implies significant difference between two or more groups; > 0.05 implies no difference
df$labels_fct <- labels_fct # or can just use labels as categorical values
kruskal.test(labels_fct ~ nums, data = df)
