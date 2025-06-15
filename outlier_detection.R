library(dplyr)
library(ggplot2)

gc()

output_folder <- "C:\\Users\\kelvi\\Desktop\\outliers\\"

# check output folder exists
if (!file.exists(paste(output_folder,sep=""))){
  dir.create(file.path(paste(output_folder,sep="")), recursive=TRUE) # recursive to also build any sub-folders
}

dat <- abs(rnorm(90))

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

ggsave(paste(output_folder,"1data_ggplot.png",sep=""), width=30, height=15, units="cm")

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

ggsave(paste(output_folder,"2data_ggplot.png",sep=""), width=30, height=15, units="cm")

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
  geom_line(color=bs_df$model)+
  theme_classic()

ggsave(paste(output_folder,"3data_ggplot.png",sep=""), width=30, height=15, units="cm")

# boxplot of models
ggplot(bs_df, aes(x=model,y=preds, group=model))+
  geom_boxplot()+
  theme_classic()

ggsave(paste(output_folder,"4data_ggplot.png",sep=""), width=30, height=15, units="cm")

# boxplot of models - top 100
ggplot(bs_df[1:1000,], aes(x=model,y=preds, group=model))+
  geom_boxplot()+
  theme_classic()

ggsave(paste(output_folder,"4data_top100_ggplot.png",sep=""), width=30, height=15, units="cm")

################################################################################

# from the lm model predictions, create a bootstrap histogram of the expected means

# credit to https://bookdown.org/jgscott/DSGI/the-bootstrap.html

# get unique model number
unq_mdl <- unique(bs_df$model)[1:5] # limit to first 5

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
  
  # get standard error (68%)
  sd_err <- sd(df_hist$t.bootdist.)
  
  # get standard error (95%)
  sd_err_95 <- 2* sd(df_hist$t.bootdist.)
  
  # get confidence interval
  #conf_int <- confint(df_hist, level=0.95)
  
  # determine how many values fall within this range
  val_count <- df_hist |>
    filter(t.bootdist. > mean_val - sd_err & t.bootdist. < mean_val + sd_err)
  
  val_count_95 <- df_hist |>
    filter(t.bootdist. > mean_val - sd_err_95 & t.bootdist. < mean_val + sd_err_95)
  
  # as pct
  pct_val_count <- round((nrow(val_count)/nrow(df_hist))*100,3)
  pct_val_count_95 <- round((nrow(val_count_95)/nrow(df_hist))*100,3)
  
  graph_title <- paste("Model ",mdl," mean: ",round(mean_val,3)," with ", pct_val_count_95, "% error lines",
                       "\nn=",length(bootdist),"; sd=",round(sd_err,3),"; IQR=",round(IQR(df_hist$t.bootdist.),3),sep="")
  
  # create histogram of this data - base R
  plot.new()
  hist(bootdist, 'FD', col=4, main=graph_title)
  abline(v=(mean_val),lty=1,col="red")
  abline(v=(mean_val+sd_err_95),lty=2,col="red")
  abline(v=(mean_val-sd_err_95),lty=2,col="red")
  dev.copy(png, paste(output_folder,"model_",mdl,"_hist.png",sep=""))
  dev.off()
  
  # create boxplot - base R
  plot.new()
  boxplot(bootdist, notch = TRUE, horizontal = TRUE, main=graph_title)
  
  dev.copy(png, paste(output_folder,"model_",mdl,"_boxplot.png",sep=""))
  dev.off()
  
  # create boxplot - base R - 10% of observations
  ten_pct <- 0.1*length(bootdist)
  plot.new()
  boxplot(bootdist[1:ten_pct], notch = TRUE, horizontal = TRUE, main=graph_title)
  abline(v=(mean_val),lty=1,col="red")
  abline(v=(mean_val+sd_err_95),lty=2,col="red")
  abline(v=(mean_val-sd_err_95),lty=2,col="red")
  dev.copy(png, paste(output_folder,"model_",mdl,"_10pc_boxplot.png",sep=""))
  dev.off()
  
  # create probability density plot - base R
  plot.new()
  hist(bootdist, freq=FALSE, col=4, main=graph_title)
  lines(density(bootdist), lwd=2)
  lines(density(bootdist, adj=.5), lwd=1)
  lines(density(bootdist, adj=2), lwd=1.5)
  abline(v=(mean_val),lty=1,col="red")
  abline(v=(mean_val+sd_err_95),lty=2,col="red")
  abline(v=(mean_val-sd_err_95),lty=2,col="red")
  dev.copy(png, paste(output_folder,"model_",mdl,"_prob.png",sep=""))
  dev.off()
  
  # create qq plot - base R
  plot.new()
  qqnorm(bootdist, main=graph_title)
  qqline(bootdist, col=4)
  dev.copy(png, paste(output_folder,"model_",mdl,"_qq.png",sep=""))
  dev.off()
  
  # create log qq plot - base R
  plot.new()
  qqnorm(log(bootdist), main=graph_title)
  qqline(log(bootdist), col=4)
  dev.copy(png, paste(output_folder,"model_",mdl,"_logqq.png",sep=""))
  dev.off()
  
  # geom_scatter - ggplot
  ggplot(df_hist,aes(x=t.bootdist., y=t.bootdist.))+
    geom_point()+
    labs(x="Preds",y="Frequency",title=graph_title)+
    theme_classic()
  
  ggsave(paste(output_folder,"model_",mdl,"scatter_ggplot.png",sep=""), width=30, height=15, units="cm")
  
  # geom_histogram - ggplot (depending upon binwidth, bins etc. can look different due to frequency distributions)
  ggplot(df_hist,aes(x=t.bootdist.))+
    geom_histogram(binwidth=0.5, fill="blue", color="black")+
    geom_density()+
    geom_vline(xintercept=mean_val+sd_err_95,color="red")+ # sd error lines
    geom_vline(xintercept=mean_val-sd_err_95,color="red")+ # sd error lines
    labs(x="Preds",y="Frequency",title=graph_title)+
    theme_classic()
  
  ggsave(paste(output_folder,"model_",mdl,"ggplot.png",sep=""), width=30, height=15, units="cm")
  
  ggplot(df_hist,aes(x=t.bootdist.,y=after_stat(density)))+
    geom_histogram(binwidth=0.5, fill="blue", color="black")+
    geom_density(bounds = c(1, Inf), color="black")+
    geom_vline(xintercept=mean_val+sd_err_95,color="red")+ # sd error lines
    geom_vline(xintercept=mean_val-sd_err_95,color="red")+ # sd error lines
    labs(x="Preds",y="Frequency",title=graph_title)+
    theme_classic()
  
  ggsave(paste(output_folder,"model_",mdl,"xtra_ggplot.png",sep=""), width=30, height=15, units="cm")
}