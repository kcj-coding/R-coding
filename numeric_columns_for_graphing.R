#library(tidyverse)
library(dplyr)
library(ggplot2)
#library(summarytools)

setwd("C:\\Users\\kelvi\\Desktop")
folder <- "C:\\Users\\kelvi\\Desktop\\Graphs\\"

data <- c(1,2,3,4,5,6,7,8,9,10)
typer <- c("a","b","a","b","a","b","a","b","a","b")

# make a dataframe
df <- data.frame(data=data, type=typer)

# get character columns (non-numeric) from df
#df_chr <- df[, sapply(df, class) == 'character']
df_chr <- df[sapply(df, class) == 'character']

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
  datad <- data.frame(dat=df_chk, fct=as.numeric(factor(df_chk)))
  
  # rename columns
  colnames(datad) <- c(name1,name2)
  
  #dfs <- cbindPad(dfs,datad)
  #dfs <- cbind(dfs, datad)
  d[[i]] <- datad
  
}

#dfs <- select(dfs, -testcol)
dfs <- do.call(cbind,d)

# write output csv of this data
write.csv(dfs,paste(folder,"output.csv",sep=""), row.names=FALSE)

# remove all character columns from original df and add on these generated columns

# Get all character columns
df <- df[sapply(df, class) != 'character']

# join on created data
df <- cbind(df, dfs)

# for graphs, use numeric columns only
df_nums <- df[sapply(df, class) == 'numeric']

for (i in seq(1, length(df_nums), 1)){
  for (j in seq(1, length(df_nums), 1)){
    if (i != j){
      ############## base R graph ##############################################
      # for plot.new() keep view on Plots tab, to avoid new window opening
      plot.new()
      plot(df_nums[,i], df_nums[,j], ylab=names(df_nums[j]),xlab=names(df_nums[i]), main=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), sep=""))
      
      # check if folder exists if not create it
      # check/create a folder
      if (!file.exists(paste(folder,names(df_nums[i]),sep=""))){
        dir.create(paste(folder,names(df_nums[i]),sep=""))
      }
      
      # save base R graph
      dev.copy(png, paste(folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"both_plot.png",sep=""))
      dev.off()
      
      ################## ggplot graph ##########################################
      ggplot(df_nums, aes(x=df_nums[,i], y=df_nums[,j]))+
        geom_point()+
        geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
        geom_smooth(method=lm, formula=y~splines::bs(x,3), se=FALSE, color="red")+
        theme_classic()+
        labs(x=names(df_nums[i]), y=names(df_nums[j]), title=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), sep=""))
      # save
      ggsave(paste(folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"ggplot_both_plot.png",sep=""),width=30,height=15,units="cm")
      
      }
  }
}

# remove locally saved plots
# list all files in folder location, and remove them all
files_del <- list.files(folder)

for (folder_name in files_del){
  unlink(paste(folder,folder_name,sep=""), recursive = TRUE)
}
