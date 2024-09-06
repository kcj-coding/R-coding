library(dplyr)
library(ggplot2)

dat <- rnorm(60)

df <- data.frame(data=dat)
df$dat <- df$dat * 1e2

mean <- mean(df$dat)
sd <- sd(df$dat)

df$chk <- ifelse(df$dat < (mean-(2*sd)), "outlier",
                 ifelse(df$dat > (mean+(2*sd)), "outlier", "expected"))

df$xdata <- as.numeric(rownames(df))

# graph this data
ggplot(df, aes(df$xdata, df$dat, color=df$chk))+
  geom_point()+
  geom_hline(yintercept=mean)+
  geom_hline(yintercept=(mean+(2*sd)), color="blue")+
  geom_hline(yintercept=(mean-(2*sd)), color="blue")+
  theme_classic()+
  labs(x="Datapoint", y="Number", color="Type")

################################################################################

# linear regression and outlier
lm1 <- lm(dat ~ xdata, data=df)

# lm1 model #############################################
a = format(unname(coef(lm1)[1]), digits = 2) # intercept
b = format(unname(coef(lm1)[2]), digits = 2) # bx term
r2 = format(summary(lm1)$r.squared, digits = 3) # r-squared

dat_tst <- data.frame(xdata=df$xdata)

# fit to df
mdl_preds <- predict(lm1, newdata=dat_tst)

# compare to actual data
df$exp <- mdl_preds

df$exp_val <- ifelse(df$dat < (df$exp*(-5)), "outlier",
                     ifelse(df$dat > (df$exp*(5)), "outlier", "expected"))

# graph this
ggplot(df, aes(xdata, dat, color=exp_val))+
  geom_point()+
  geom_line(aes(df$xdata,df$exp), color="black")+
  geom_line(aes(df$xdata,abs(df$exp)*5), color="blue")+
  geom_line(aes(df$xdata,-(df$exp)*5), color="blue")+
  theme_classic()+
  labs(x="Datapoint", y="Number", title=paste("LR: ", "y=",b,"x+",a," r2=",r2,sep=""), color="Type")

