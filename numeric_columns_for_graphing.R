#library(tidyverse)
library(dplyr)
library(ggplot2)
#library(summarytools)

setwd("C:\\Users\\kelvi\\Desktop")
folder <- "C:\\Users\\kelvi\\Desktop\\"
output_folder <- "C:\\Users\\kelvi\\Desktop\\Graphs\\"

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

# fill any numeric NA with 0
na.zero <- function(x) replace(x, is.na(x), 0)
ok <- sapply(df, is.numeric)
df <- replace(df, ok, lapply(df[ok], na.zero))

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


# then plot individual numeric data

for (i in seq(1, length(df_nums), 1)){
  for (j in seq(1, length(df_nums), 1)){
    if (i != j){
      ##########################################################################
      
      ############# linear equation examples for graph #########################
      lm1 <- lm(df_nums[,j] ~ df_nums[,i])
      lm2 <- lm(df_nums[,j] ~ poly(df_nums[,i],3,raw=TRUE))
      
      mod <- data.frame(vals=lm1$fitted.values, resid=lm1$residuals, actuals=df_nums[,j])
      
      # lm1 model #############################################
      a = format(unname(coef(lm1)[1]), digits = 2) # intercept
      b = format(unname(coef(lm1)[2]), digits = 2) # bx term
      r2 = format(summary(lm1)$r.squared, digits = 3) # r-squared
      
      # lm2 model #############################################
      ap = format(unname(coef(lm2)[1]), digits = 2) # intercept
      bp = format(unname(coef(lm2)[2]), digits = 2) # bx term
      bp2 = format(unname(coef(lm2)[3]), digits = 2) # bx2 term
      bp3 = format(unname(coef(lm2)[4]), digits = 2) # bx3 term
      r2p = format(summary(lm2)$r.squared, digits = 3) # r-squared
      
      ############## base R graph ##############################################
      # for plot.new() keep view on Plots tab, to avoid new window opening
      plot.new()
      plot(df_nums[,i], df_nums[,j], ylab=names(df_nums[j]),xlab=names(df_nums[i]), main=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), sep=""))
      
      # check if folder exists if not create it
      # check/create a folder
      if (!file.exists(paste(output_folder,names(df_nums[i]),sep=""))){
        dir.create(paste(output_folder,names(df_nums[i]),sep=""), recursive=TRUE) # recursive to also build any sub-folders
      }
      
      # save base R graph
      dev.copy(png, paste(output_folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"both_plot.png",sep=""))
      dev.off()
      
      ##########################################################################
      
      ################## ggplot graph ##########################################
      ggplot(df_nums, aes(x=df_nums[,i], y=df_nums[,j]))+
        geom_point(alpha=0.1)+
        geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
        geom_smooth(method=lm, formula=y~splines::bs(x,3), se=FALSE, color="red")+
        theme_classic()+
        labs(x=names(df_nums[i]), y=names(df_nums[j]), title=paste("Plot of ",names(df_nums[i]), " by ", names(df_nums[j]), " || LR: ", "y=",b,"x+",a," r2=",r2, " || PY: ", "y=",bp,"x+",bp2,"x2+",bp3,"x3+",ap," r2=",r2p,sep=""))+
        theme(plot.title = element_text(size=12))
      # save
      ggsave(paste(output_folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"ggplot_both_plot.png",sep=""),width=30,height=15,units="cm", dpi=128)
      
      ########################## lm model residuals graph ######################
      
      ggplot(mod, aes(x=vals, y=actuals))+
        geom_point(alpha=0.1)+
        geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
        theme_classic()+
        labs(x=names(df_nums[i]), y=names(df_nums[j]), title=paste("Plot of residuals by ", names(df_nums[j]), " || LR: ", "y=",b,"x+",a," r2=",r2,sep=""))+
        theme(plot.title = element_text(size=12))
      # save
      ggsave(paste(output_folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"ggplot_residuals.png",sep=""),width=30,height=15,units="cm", dpi=128)
      
      
      ##########################################################################
      
      ####################### k-means clustering ###############################
      
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
        ggsave(paste(output_folder,names(df_nums[i]),"\\",names(df_nums[i]),"_",names(df_nums[j]),"_cluster_ggplot_both_plot.png",sep=""),width=30,height=15,units="cm", dpi=128)
      }, error = function(e) e)
    }
  }
}

# remove locally saved plots
# list all files in folder location, and remove them all
files_del <- list.files(output_folder)

for (folder_name in files_del){
  unlink(paste(output_folder,folder_name,sep=""), recursive = TRUE)
}