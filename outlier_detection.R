library(dplyr)
library(ggplot2)

gc()

output_folder <- "C:\\Users\\kelvi\\Desktop\\"

dat <- rnorm(90)

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

# get mape, mae, rmse
mdl_mape <- mean(abs((df$dat-df$exp)/df$dat)) * 100

mdl_mae <- mean(abs((df$dat-df$exp)))

mdl_rmse <- sqrt(mean(abs((df$dat - df$exp)^2)))

# graph this
ggplot(df, aes(xdata, dat, color=exp_val))+
  geom_point()+
  geom_line(aes(df$xdata,df$exp), color="black")+
  geom_line(aes(df$xdata,abs(df$exp)*5), color="blue")+
  geom_line(aes(df$xdata,-(df$exp)*5), color="blue")+
  theme_classic()+
  labs(x="Datapoint", y="Number", title=paste("LR: ", "y=",b,"x+",a," r2=",r2,sep=""), color="Type")

################################################################################

# credit to https://stackoverflow.com/questions/74295914/bootstrap-distribution-of-slope-on-linear-regression

# make a bootstrap of the model (automatic method)
library(boot)

boot_coef <- function(df, idx) {
  x <- df[ idx, 'xdata' ]
  y <- df[ idx, 'dat' ]
  lm.out <- lm(y ~ x)
  coef(lm.out)[[ 2 ]]
}

bs <- boot(data = df, statistic = boot_coef, R = 999)
print(bs)
bs.ci <- boot.ci(boot.out = bs)
print(bs.ci)

#########################

# alternative bootstrap of model (manual method)

FUN <- \() {
  i <- sample(seq_len(length(df$xdata)), replace=TRUE)
  y <- df$dat[i]
  x <- df$xdata[i]
  lm(y ~ x)$coefficients
  #predict(lm, newdata=dat_tst)
}

set.seed(42)
r <- t(replicate(1e3, FUN()))

head(r, 3)

bootdist <- r[, 'x']

head(bootdist)

hist(bootdist, 'FD', col=4)

# for each model coefficents created, predict new values
df_r <- data.frame(r)

bs_res <- list()
for (i in 1:nrow(df_r)){
  intercept <- df_r[[i,1]]
  xval <- df_r[[i,2]]
  
  lm1$coefficients[1:2] <- c(intercept, xval)
  print(coef(lm1))
  vals <- predict(lm1, newdata=dat_tst)
  dff <- data.frame(model=i,preds=vals)
  dff$xval <- seq.int(nrow(dff))
  bs_res[[i]] <- dff
}

bs_df <- do.call(rbind, bs_res)

# graph these models
ggplot(bs_df, aes(x=xval,y=preds, group=model))+
  geom_line(color=bs_df$model)

# boxplot of models
ggplot(bs_df, aes(x=model,y=preds, group=model))+
  geom_boxplot()

################################################################################

# from the lm model predictions, create a bootstrap histogram of the expected means

# credit to https://bookdown.org/jgscott/DSGI/the-bootstrap.html

# get unique model number
unq_mdl <- unique(bs_df$model)

for (mdl in unq_mdl) {
  bs_df_tst <- bs_df |> filter(model == mdl)
  
  mean_val <- mean(bs_df_tst$preds)
  
  # bootstrap the mean values
  FUN <- \() {
    i <- sample(seq_len(length(bs_df_tst$model)), replace=TRUE)
    x <- mean(bs_df_tst$preds[i])

  }
  
  set.seed(42)
  r <- t(replicate(1e3, FUN()))
  
  head(r, 3)
  
  bootdist <- r#[, 'x']
  
  # create df of this for geom_histogram
  df_hist <- data.frame(t(bootdist))
  
  # get standard error
  sd_err <- sd(df_hist$t.bootdist.)
  
  # get confidence interval
  #conf_int <- confint(df_hist, level=0.95)
  
  # determine how many values fall within this range
  val_count <- df_hist |>
    filter(t.bootdist. > mean_val - sd_err & t.bootdist. < mean_val + sd_err)
  
  # as pct
  pct_val_count <- round((nrow(val_count)/nrow(df_hist))*100,3)
  
  # create histogram of this data - base R
  plot.new()
  hist(bootdist, 'FD', col=4, main=paste("Model ",mdl," mean: ",round(mean_val,3),sep=""))
  dev.copy(png, paste(output_folder,"model_",mdl,".png",sep=""))
  dev.off()
  
  # geom_histogram - ggplot (depending upon binwidth, bins etc. can look different due to frequency distributions)
  ggplot(df_hist,aes(x=t.bootdist.))+
    geom_histogram(binwidth=0.5, fill="blue", color="black")+
    geom_vline(xintercept=mean_val+sd_err,color="red")+ # sd error lines
    geom_vline(xintercept=mean_val-sd_err,color="red")+ # sd error lines
    labs(x="Preds",y="Frequency",title=paste("Model ",mdl," mean: ",round(mean_val,3)," with ", pct_val_count, "% error lines",sep=""))+
    theme_classic()
  
  ggsave(paste(output_folder,"model_",mdl,"ggplot.png",sep=""), width=30, height=15, units="cm")
}

