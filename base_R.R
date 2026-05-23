
gc()
start_time <- Sys.time()

folder <- r"[C:\Users\kelvi\Desktop\xyz]"
output_folder <- r"[C:\Users\kelvi\Desktop\xyz\Graphs]"

folder_locations <- c(folder, output_folder)

# check output folder exists
for (folder_location in folder_locations){
  if (!file.exists(paste(folder_location,sep=""))){
    dir.create(file.path(paste(folder_location,sep="")), recursive=TRUE) # recursive to also build any sub-folders
  }
}

##################################################

number_format <- function(num){
  if (is.na(num)){
    digits <- 0
  }
  else if (num > 0){
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
  else if (digits <= 3 & !is.na(num)){
    value <- format(num, digits=2)
  }
  else {
    value <- 0
  }
  return (value)
}

##################################################

data <- 1:10#c(1,2,3,4,5,6,7,8,9,10)
typer <- rep(c("a","b"),times=5)#c("a","b","a","b","a","b","a","b","a","b")

# make a dataframe
df <- data.frame(data=data, type=typer)

# or read.csv
#df <- read.csv(file, stringsAsFactors=FALSE)

# write data summary
varsum <- summary(df)#data.frame(sapply(df, function(x) c(summary(x))))#, type = class(x)))
write.csv(varsum, file = paste(folder,"\\",'varsum.csv',sep=""),row.names=FALSE)

# rename columns
colnames(df)[1] <- "numbers"
colnames(df)[2] <- "letters"

df[[1]] <- as.integer(df[[1]])

# make date column
df$dater <- as.Date(df$numbers, origin="1970-01-01")

###################################################

# group by methods
dfx <- aggregate(numbers~letters, data=df, FUN=sum)

dfx1 <- tapply(df$numbers, df$letters, sum)

dfx2 <- by(df$numbers, df$letters, sum)

df1 <- split(df$numbers, df$letters)
df2 <- lapply(df1, sum)

dfxx <- data.frame(groups=names(df1), values=sapply(df1,sum))

##################################################


# case when and if else
df <- within(df, {
  y=NA
  y[numbers <= 3]="aaa"
  y[numbers > 3 & numbers <= 6]="bbb"
})

# if else
df$yy <- ifelse(is.na(df$y), "ccc", df$y)

##################################################

# filter df
df2 <- df[df$y=="bbb",]

# get all non numeric columns
df_chr <- df[!sapply(df, is.numeric)]

# get 4th text space from example sentence
text <- "abc def ghi jkl"
txt <- sub("^([^\\s]+\\s+){3}([^\\s]+).*", "\\2", text, perl=TRUE)

##################################################

# graphs

# get sd and mean val
vv <- df[[1]] # column numbers or df$numbers
mean <- mean(vv)
mean_val <- mean(vv)
sd <- sd(vv)
iqr1 <- IQR(vv)

# get standard error (68%)
sd_err <- sd(vv)

# get standard error (95%)
sd_err_95 <- 2* sd(vv)

# determine how many values fall within this range
val_count <- df[df[1] > mean_val - sd_err & df[1] < mean_val + sd_err,]

val_count_95 <- df[df[1] > mean_val - sd_err & df[1] < mean_val + sd_err,]


# as pct
pct_val_count <- round((nrow(val_count)/nrow(df))*100,3)
pct_val_count_95 <- round((nrow(val_count_95)/nrow(df))*100,3)


graph_title <- paste(names(df[1])," mean: ",number_format(mean_val)," with ", pct_val_count_95, "% error lines",
                     "\nn=",length(vv),"; sd=",round(sd_err,3),"; IQR=",round(IQR(vv),3),sep="")

# create boxplot - base R
plot.new()
boxplot(vv, notch = TRUE, horizontal = TRUE, main=graph_title)
abline(v=mean_val,lty=1,col="red")
abline(v=mean_val+sd_err_95,lty=2,col="red")
abline(v=mean_val-sd_err_95,lty=2,col="red")
dev.copy(png, filename=paste(output_folder,"//","_boxplot.png",sep=""), width=30, height=15, units="cm", res=300)
dev.off()

# create dotplot - base R
plot.new()
dotchart(vv, main=graph_title)
abline(v=(mean_val),lty=1,col="red")
abline(v=(mean_val+sd_err_95),lty=2,col="red")
abline(v=(mean_val-sd_err_95),lty=2,col="red")
dev.copy(png, filename=paste(output_folder,"//","_dotplot.png",sep=""), width=30, height=15, units="cm", res=300)
dev.off()

# create probability density plot - base R
plot.new()
hist(vv, freq=FALSE, col=4, main=graph_title, bty = "l")
lines(density(vv), lwd=2)
abline(v=mean_val,lty=1,col="red")
abline(v=mean_val+sd_err_95,lty=2,col="red")
abline(v=mean_val-sd_err_95,lty=2,col="red")
#lines(density(vv, adj=.5), lwd=1)
#lines(density(vv, adj=2), lwd=1.5)
dev.copy(png, filename=paste(output_folder,"//","_prob.png",sep=""), width=30, height=15, units="cm", res=300)
dev.off()

# create qq plot - base R
plot.new()
qqnorm(vv, main=graph_title, bty = "l")
qqline(vv, col=4)
dev.copy(png, filename=paste(output_folder,"//","_qq.png",sep=""), width=30, height=15, units="cm", res=300)
dev.off()

##################################################


end_time <- Sys.time()
print(paste("runtime: ",end_time-start_time,sep=""))

##################################################

# remove locally saved plots
# list all files in folder location, and remove them all
files_del <- list.files(output_folder, pattern = "\\.png$", recursive = TRUE)

for (folder_name in files_del){
   unlink(paste(output_folder,"\\",folder_name,sep=""), recursive = TRUE)
}