#library(tidyverse)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
#library(summarytools)

gc()
start_time <- Sys.time()
run_kmeans <- FALSE

corr_threshold <- 0.001

#setwd("C:\\Users\\kelvi\\Desktop")
folder <- r"[C:\Users\kelvi\Desktop\xyz]"
output_folder <- r"[C:\Users\kelvi\Desktop\xyz\Graphs]"

folder_locations <- c(folder, output_folder)

# check output folder exists
for (folder_location in folder_locations){
  if (!file.exists(paste(folder_location,sep=""))){
    dir.create(file.path(paste(folder_location,sep="")), recursive=TRUE) # recursive to also build any sub-folders
  }
}

number_format <- function(num){
  if (num > 0){
    digits <- floor(log10(num)) + 1
  }
  else if (num == 0){
    digits <- 1
  }
  else{
    digits <- floor(log10(abs(num))) + 1
  }

  if (digits > 3 | digits < 0){
    value <- formatC(num, format = "e", digits = 2)
    value <- sprintf("%.1e", num)
  }
  else {
    value <- format(num, digits=2)
  }
  return (value)
}
tst <- number_format(505.5678)
tst1 <- number_format(0.000012345)
data <- 1:10#c(1,2,3,4,5,6,7,8,9,10)
typer <- rep(c("a","b"),times=5)#c("a","b","a","b","a","b","a","b","a","b")

# make a dataframe
df <- data.frame(data=data, type=typer)

# write data summary
varsum <- summary(df)#data.frame(sapply(df, function(x) c(summary(x))))#, type = class(x)))
write.csv(varsum, file = paste(folder,"\\",'varsum.csv',sep=""),row.names=FALSE)

# get all non numeric columns
df_chr <- df[sapply(df, class) != 'numeric']

# if the length of df_chr > 0
if (length(df_chr) > 0){

  # fill any chr NA with 0
  chr.zero <- function(x) replace(x, is.na(x), "Unknown")
  ok <- sapply(df_chr, is.character)
  df_chr <- replace(df_chr, ok, lapply(df_chr[ok], chr.zero))
  
  # function from https://stackoverflow.com/questions/6988184/combining-two-data-frames-of-different-lengths
  cbindPad <- function(...){
    args <- list(...)
    n <- sapply(args,nrow)
    mx <- max(n)
    pad <- function(x, mx){
      if (nrow(x) < mx){
        nms <- colnames(x)
        padTemp <- matrix(NA, mx - nrow(x), ncol(x))
        colnames(padTemp) <- nms
        if (ncol(x)==0) {
          return(padTemp)
        } else {
          return(rbind(x,padTemp))
        }
      }
      else{
        return(x)
      }
    }
    rs <- lapply(args,pad,mx)
    return(do.call(cbind,rs))
  }
  
  # for these, make 2 new columns in df which represent the character and factor conversions
  #dfs = data.frame()
  #dfs <- data.frame(testcol=1:nrow(df_chr))
  d <- list()
  for (i in seq(1, length(df_chr),1)){
    df_chk <- df_chr[,i]
    name1 <- names(df_chr[i])
    name2 <- paste(name1,"_fct",sep="")
    # get character and factor, and append to df
    datad <- data.frame(dat=df_chk[[1]], fct=as.numeric(factor(df_chk[[1]])))
    
    # rename columns
    colnames(datad) <- c(name1,name2)
    
    #dfs <- cbindPad(dfs,datad)
    #dfs <- cbind(dfs, datad)
    d[[i]] <- datad
    
  }
  
  #dfs <- select(dfs, -testcol)
  dfs <- do.call(cbind,d)
  
  # remove all character columns from original df and add on these generated columns
  
  # Get all numeric columns
  df <- df[sapply(df, class) == 'numeric']
  
  # join on created data
  df <- cbind(df, dfs)
}

# fill any numeric NA with 0
na.zero <- function(x) replace(x, is.na(x), 0)
ok <- sapply(df, is.numeric)
df <- replace(df, ok, lapply(df[ok], na.zero))

# write output csv of this data
write.csv(df,paste(folder,"//","output.csv",sep=""), row.names=FALSE)
varsum <- summary(df)#data.frame(sapply(df, function(x) c(summary(x))))#, type = class(x)))
write.csv(varsum, file = paste(folder,"\\",'varsum_fct.csv',sep=""),row.names=FALSE)

# for graphs, use numeric columns only
df_nums <- df[sapply(df, class) == 'numeric']

# plot all numeric data on histograms
library(tidyr)
hists <- df_nums |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

ggplot(hists, aes(x=value))+
  geom_histogram()+
  facet_wrap(~variable, scales="free", ncol=4)+
  theme_classic()

ggsave(paste(folder,"\\","ggplot.png",sep=""), width=30, height=15, units="cm", dpi=128)

# get correlations
#cor
#df_cor <- cor(tt)
#dfcor1 <- data.frame(row=rownames(tt)[row(tt)], col=colnames(tt)[col(tt)], corr=c(tt))
#dfcor11 <- data.frame(row=colnames(tt)[col(tt)], col=rownames(tt)[row(tt)], corr=c(tt))

d2 <- df_nums %>% 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(var1 != var2)

write.csv(d2,paste(folder,"\\","corr.csv",sep=""))

# then plot individual numeric data, based on correlations

for (i in seq(1, length(df_nums), 1)){
  
  # plot graphs of individual columns
  
  # check output folder exists
  if (!file.exists(paste(output_folder,"//","//",names(df_nums[i]),"//","column data",sep=""))){
    dir.create(file.path(paste(output_folder,"//",names(df_nums[i]),"//","column data",sep="")), recursive=TRUE) # recursive to also build any sub-folders
  }
  
  # get sd and mean val
  vv <- df_nums[[i]]
  mean <- mean(vv)
  mean_val <- mean(vv)
  sd <- sd(vv)
  
  # get standard error (68%)
  sd_err <- sd(vv)
  
  # get standard error (95%)
  sd_err_95 <- 2* sd(vv)
  
  # get confidence interval
  #conf_int <- confint(df_hist, level=0.95)
  
  # determine how many values fall within this range
  val_count <- df_nums |>
    filter(df_nums[i] > mean_val - sd_err & df_nums[i] < mean_val + sd_err)
  
  val_count_95 <- df_nums |>
    filter(df_nums[i] > mean_val - sd_err_95 & df_nums[i] < mean_val + sd_err_95)
  
  # as pct
  pct_val_count <- round((nrow(val_count)/nrow(df_nums))*100,3)
  pct_val_count_95 <- round((nrow(val_count_95)/nrow(df_nums))*100,3)
  
  # create histogram of this data - base R
  #plot.new()
  #hist(vv, 'FD', col=4, main=paste(names(df_nums[i])," mean: ",round(mean_val,3),sep=""))
  #dev.copy(png, paste(output_folder,names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"_hist.png",sep=""))
  #dev.off()
  
  # create boxplot - base R
  plot.new()
  boxplot(vv, notch = TRUE, horizontal = TRUE, main=paste(names(df_nums[i])," mean: ",number_format(mean_val),sep=""))
  dev.copy(png, paste(output_folder,"//",names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"_boxplot.png",sep=""))
  dev.off()
  
  # create boxplot - base R - 10% of observations
  #ten_pct <- 0.1*length(df_nums[i])
  #plot.new()
  #boxplot(vv[1:ten_pct], notch = TRUE, horizontal = TRUE, main=paste("Model "," mean: ",round(mean_val,3),sep=""))
  #dev.copy(png, paste(output_folder,names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"_10pc_boxplot.png",sep=""))
  #dev.off()
  
  # create probability density plot - base R
  plot.new()
  hist(vv, freq=FALSE, col=4, main=paste(names(df_nums[i])," mean: ",number_format(mean_val)," with ", pct_val_count_95, "% error lines", sep=""))
  lines(density(vv), lwd=2)
  abline(v=mean_val+sd_err_95,lty=2,col="red")
  abline(v=mean_val-sd_err_95,lty=2,col="red")
  #lines(density(vv, adj=.5), lwd=1)
  #lines(density(vv, adj=2), lwd=1.5)
  dev.copy(png, paste(output_folder,"//",names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"_prob.png",sep=""))
  dev.off()
  
  # create qq plot - base R
  plot.new()
  qqnorm(vv, main=paste(names(df_nums[i])," mean: ",number_format(mean_val),sep=""))
  qqline(vv, col=4)
  dev.copy(png, paste(output_folder,"//",names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"_qq.png",sep=""))
  dev.off()
  
  #tryCatch({
  # create log qq plot - base R
  #plot.new()
  #qqnorm(log(vv), main=paste("Model "," mean: ",round(mean_val,3),sep=""))
  #qqline(log(vv), col=4)
  #dev.copy(png, paste(output_folder,names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"_logqq.png",sep=""))
  #dev.off()
  #}, error = function(e) e)
  
  # geom_scatter - ggplot
  #ggplot(df_nums,aes(x=df_nums[[i]], y=df_nums[[i]]))+
  #  geom_point()+
  #  labs(x="Preds",y="Frequency",title=paste("Model ",sep=""))+
  #  theme_classic()
  
  #ggsave(paste(output_folder,names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"scatter_ggplot.png",sep=""), width=30, height=15, units="cm", dpi=128)
  
  # geom_histogram - ggplot (depending upon binwidth, bins etc. can look different due to frequency distributions)
  #ggplot(df_nums,aes(x=df_nums[[i]]))+
  #  geom_histogram(binwidth=0.1*mean_val, fill="blue", color="black", stat="count")+
  #  #geom_density()+
  #  geom_vline(xintercept=mean_val+sd_err_95,color="red")+ # sd error lines
  #  geom_vline(xintercept=mean_val-sd_err_95,color="red")+ # sd error lines
  #  labs(x="Preds",y="Frequency",title=paste("Model "," mean: ",round(mean_val,3)," with ", pct_val_count_95, "% error lines",sep=""))+
  #  theme_classic()
  
  #ggsave(paste(output_folder,names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"ggplot.png",sep=""), width=30, height=15, units="cm", dpi=128)
  
  #ggplot(df_nums, aes(x=df_nums[[i]],y=df_nums[[i]]))+
  #  geom_histogram(binwidth=0.5, fill="blue", color="black", stat="count")+
  #  geom_density(bounds = c(1, Inf), color="black")+
  #  geom_vline(xintercept=mean_val+sd_err_95,color="red")+ # sd error lines
  #  geom_vline(xintercept=mean_val-sd_err_95,color="red")+ # sd error lines
  #  labs(x="Preds",y="Frequency",title=paste("Model "," mean: ",round(mean_val,3)," with ", pct_val_count_95, "% error lines",sep=""))+
  #  theme_classic()
  
  #ggsave(paste(output_folder,names(df_nums[i]),"\\","column data","\\",names(df_nums[i]),"xtra_ggplot.png",sep=""), width=30, height=15, units="cm")
  
  for (j in seq(1, length(df_nums), 1)){
    # plot graphs of column against other columns if correlation is not na
    name1 <- as.character(names(df_nums[i]))
    name2 <- as.character(names(df_nums[j]))
    if (i != j){
      corr_scor <- d2[(d2$var2==name1 & d2$var1==name2),]
      corr_scor <- corr_scor$value[[1]]
      if ((i != j & !is.na(corr_scor)) & (corr_threshold >= abs(corr_scor))){
        ##########################################################################
        
        ############# linear equation examples for graph #########################
        lm1 <- lm(df_nums[,j] ~ df_nums[,i])
        lm2 <- lm(df_nums[,j] ~ poly(df_nums[,i],3,raw=TRUE))
        
        mod <- data.frame(vals=lm1$fitted.values, resid=lm1$residuals, actuals=df_nums[,j])
        
        # lm1 model #############################################
        a = unname(coef(lm1)[1])#format(unname(coef(lm1)[1]), digits = 2) # intercept
        b = unname(coef(lm1)[2])#format(unname(coef(lm1)[2]), digits = 2) # bx term
        r2 = summary(lm1)$r.squared#format(summary(lm1)$r.squared, digits = 3) # r-squared
        
        # lm2 model #############################################
        ap = unname(coef(lm2)[1])#format(unname(coef(lm2)[1]), digits = 2) # intercept
        bp = unname(coef(lm2)[2])#format(unname(coef(lm2)[2]), digits = 2) # bx term
        bp2 = unname(coef(lm2)[3])#format(unname(coef(lm2)[3]), digits = 2) # bx2 term
        bp3 = unname(coef(lm2)[4])#format(unname(coef(lm2)[4]), digits = 2) # bx3 term
        r2p = summary(lm2)$r.squared#format(summary(lm2)$r.squared, digits = 3) # r-squared
        
        ############## base R graph ##############################################
        # for plot.new() keep view on Plots tab, to avoid new window opening
        #plot.new()
        # plot(df_nums[,i], df_nums[,j], ylab=names(df_nums[j]),xlab=names(df_nums[i]), main=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), sep=""))
        
        # check if folder exists if not create it
        # check/create a folder
        if (!file.exists(paste(output_folder,names(df_nums[i]),sep=""))){
          dir.create(paste(output_folder,"//",names(df_nums[i]),sep=""), recursive=TRUE) # recursive to also build any sub-folders
        }
        
        # save base R graph
        #dev.copy(png, paste(output_folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"both_plot.png",sep=""))
        #dev.off()
        
        ##########################################################################
        
        ################## ggplot graph ##########################################
        ggplot(df_nums, aes(x=df_nums[,i], y=df_nums[,j]))+
          geom_point(alpha=0.1)+
          geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
          geom_smooth(method=lm, formula=y~splines::bs(x,3), se=FALSE, color="red")+
          theme_classic()+
          labs(x=names(df_nums[i]), y=names(df_nums[j]), title=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), " || LR: ", "y=",number_format(b),"x+",number_format(a)," r2=",number_format(r2), " || PY: ", "y=",number_format(bp),"x+",number_format(bp2),"x2+",number_format(bp3),"x3+",number_format(ap)," r2=",number_format(r2p),"\n cor threshold: ",corr_threshold, "; actual corr: ",number_format(abs(corr_scor)),sep=""))+
          theme(plot.title = element_text(size=12))
        # save
        ggsave(paste(output_folder,"//",names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"ggplot_both_plot.png",sep=""),width=30,height=15,units="cm", dpi=128)
        
        ########################## lm model residuals graph ######################
        
        #ggplot(mod, aes(x=vals, y=actuals))+
        #  geom_point(alpha=0.1)+
        #  geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
        #  theme_classic()+
        #  labs(x=names(df_nums[i]), y=names(df_nums[j]), title=paste("Plot of residuals by ", names(df_nums[j]), " || LR: ", "y=",b,"x+",a," r2=",r2,sep=""))+
        #  theme(plot.title = element_text(size=12))
        # save
        #ggsave(paste(output_folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"ggplot_residuals.png",sep=""),width=30,height=15,units="cm", dpi=128)
        
        
        ##########################################################################
        
        ####################### k-means clustering ###############################
        if(run_kmeans==TRUE){
          tryCatch({
            # scale the data
            dfx <- df_nums[,c(i,j)]
            dfx[,c(1,2)] <- scale(dfx[,c(1,2)])
            
            set.seed(123)
            n_clusters <- 5 # number of clusters to use
            
            use_nbclust <- FALSE
            # or use NbClust to get optimum nbr of clusters
            if (use_nbclust == TRUE){
              library(NbClust)
              res <- NbClust(dfx, method = 'complete', index = 'all',min.nc=1, max.nc=10)
              res <- res$Best.nc
              
              n_clusters <- res
            }
            
            # build k-means model
            km.out <- kmeans(dfx, centers = n_clusters, nstart = 20)
            
            dfx$cluster_id <- factor(km.out$cluster)
            
            ggplot(dfx, aes(x=dfx[,1], y=dfx[,2], color = cluster_id))+
              geom_point(alpha=0.1)+
              theme_classic()+
              labs(x=names(df_nums[i]), y=names(df_nums[j]), title=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), sep=""))+
              theme(plot.title = element_text(size=12))
            # save
            ggsave(paste(output_folder,"//",names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"_cluster_ggplot_both_plot.png",sep=""),width=30,height=15,units="cm", dpi=128)
          }, error = function(e) e)}
      }
    }
  }
}


end_time <- Sys.time()
print(paste("runtime: ",end_time-start_time,sep=""))

# remove locally saved plots
# list all files in folder location, and remove them all
files_del <- list.files(output_folder, pattern = "\\.png$", recursive = TRUE)

for (folder_name in files_del){
  unlink(paste(output_folder,folder_name,sep=""), recursive = TRUE)
}