
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

data <- runif(n=100,min=1,max=10) #,6,7,8,9,10)
typer <- rep(c("a","b"),times=50) #"a","b","a","b","a","b","a","b","a","b")

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
dfx <- aggregate(numbers~letters, data=df, FUN=sum) # can also add conditions e.g. numbers~letters+dater

dfx1 <- tapply(df$numbers, df$letters, sum)

dfx2 <- by(df$numbers, df$letters, sum)

df1 <- split(df$numbers, df$letters)
df2 <- lapply(df1, sum)

dfxx <- data.frame(groups=names(df1), values=sapply(df1,sum))

# can also group by and add to new df e.g.
dfx1 <- dfx
colnames(dfx1) <- c("letters","Num")
df_join <- merge(df,dfx1,by=c("letters"), all.x = TRUE)
df_join <- merge(df,dfx1,by.x="letters",by.y="letters")

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

##########################

filename <- "C:\\Folder\\abc def 05-06-2026.txt"
# get date from filename
dater <- gsub("[a-zA-Z\\\\_\\.\\:\\s]","",filename,perl=TRUE)
# get everything except date
nodater <- gsub("[0-9]","",filename,perl=TRUE)
#nodater <- gsub("(?<=\\.).*(?=\\.txt)","",filename,perl=TRUE)

########################

# or str_extract like without stringr
pattern <- "(?<=\\s)[0-9\\-].*(?=\\.)"
match_pattern <- gregexpr(pattern,filename,ignore.case=TRUE,perl=TRUE,fixed=FALSE)
dater1 <- regmatches(filename,match_pattern)[[1]]

# or mapply to dataframe to get matches e.g. df <- 
filter_df <- mapply(regmatches,df$text)
#filtered_df <- filtered_df[match_positions function(m){m !=-1},]

filtered_df <- sapply(match_pattern, function(m)any(m !=-1))

filtered_df <- df[sapply(match_pattern, function(x) x[1] != -1), ]

##################################################

# read xml file
xml <- "C:\\Windows\\WinSxS\\migration.xml"
xml_file <- read.delim(xml, header=FALSE, sep="\t") 
colnames(xml_file) <- "text"

# extract
xml_pattern <- '<file>(.*?)</file>'
xml_matches <- gregexpr(xml_pattern,xml_file$text,ignore.case=TRUE,perl=TRUE,fixed=FALSE) # this gives start position of match as int and the match length as int
xml1 <- regmatches(xml_file$text,xml_matches)

# or as one big string piece to get count of words
xml2 <- as.character(paste(xml1, collapse=" "))
xml2 <- gsub("[^\\sa-zA-Z0-9]", " ", xml2, perl=TRUE)
# count of words
words <- strsplit(xml2, "\\s")
words1 <- unique(words)

ttt <- data.frame(text=words,num=1)
namer <- colnames(ttt)[1]
colnames(ttt)[colnames(ttt)==namer] <- "text"
colnames(ttt) <- c("text","num")
ttt$text <- as.character(ttt$text)
ttt <- aggregate(ttt$num~ttt$text, FUN=sum)
colnames(ttt) <- c("text","num")
ttt <- ttt[order(ttt$num, decreasing=TRUE),]
ttt <- ttt[ttt$text != "",]


##################################################

# jitter function - to make similar view to dotchart
relative_jitter <- function(x){
  # if length is 0 return x
  if(length(x) == 0)
    return(x)
  
  # check if is numeric
  if(!is.numeric(x))
    stop("'x' must be numeric")
  
  # get range of values of x
  z <- max(x)-min(x)
  
  # get range of values to highest power of 10
  d <- unique(round(x,3-floor(log10(z))))/10
  
  amount <- 1/5*abs(d)
  
  x1 <- abs(x + stats::runif(n=length(x), min=-amount, max=amount))
  #return (x1)
}

# apply to df
df$jit <- mapply(relative_jitter,df$numbers)

##################################################

# base r colour gradient - function from https://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
color.gradient <- function(x, colors=c("purple", "yellow", "red"), 
                           colsteps=100) {
  return(colorRampPalette(colors)(colsteps)[
    findInterval(x, seq(min(x,na.rm = TRUE), max(x, na.rm = TRUE), length.out=colsteps))
  ])
}

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

# create scatterplot like dotchart - base R
ff <- mapply(relative_jitter,vv)
plot.new()
plot(ff, pch=19, cex=1, col=color.gradient(ff), xlab="", ylab="", main=graph_title, bty = "l")
abline(h=(mean_val),lty=1,col="red")
abline(h=(mean_val+sd_err_95),lty=2,col="red")
abline(h=(mean_val-sd_err_95),lty=2,col="red")
dev.copy(png, filename=paste(output_folder,"//","_scatter_like_dotchart.png",sep=""), width=30, height=15, units="cm", res=300)
dev.off()

# create scatterplot like dotchart - base R
ff <- mapply(relative_jitter,vv)
plot.new()
plot(x=ff,y=1:length(vv), pch=19, cex=1, col=color.gradient(ff), xlab="", ylab="", main=graph_title, bty = "l")
abline(v=(mean_val),lty=1,col="red")
abline(v=(mean_val+sd_err_95),lty=2,col="red")
abline(v=(mean_val-sd_err_95),lty=2,col="red")
dev.copy(png, filename=paste(output_folder,"//","_scatter_like_dotchart1.png",sep=""), width=30, height=15, units="cm", res=300)
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

# create heatmap
df_a <- df[df$letters=="a",]
df_b <- df[df$letters=="b",]

# get numbers and letters, cbind
df_a <- df_a[c("numbers","letters")]
df_b <- df_b[c("numbers","letters")]

df_ab <- cbind(df_a,df_b)
colnames(df_ab) <- c("numbers_a","letters_a","numbers_b","letters_b")

# get percentage each contributes to total as ratio
df_ab$pct <- abs((df_ab$numbers_a-df_ab$numbers_b)/10)
df_ab$pick <- ifelse(df_ab$numbers_a>df_ab$numbers_b,"a","b")
df_ab$contrib <- ifelse(df_ab$numbers_a>df_ab$numbers_b,10-df_ab$numbers_a,10-df_ab$numbers_b) # distance from 10 (max number)
df_ab$n <- 1:nrow(df_ab)

# group by contrib
df_abx <- aggregate(contrib~pick, data=df_ab, FUN=sum) 
df_abx$pct <- round(df_abx$contrib/sum(df_abx$contrib),2)
df_abx$contrib_txt <- 1:nrow(df_abx)

# make heatmap function

heatmapper <- function(x_vals,y_vals,text_vals,xlab,ylab,title){
  
  # Get unique labels for the x and y axes
  x_labels <- sort(unique(x_vals))
  y_labels <- sort(unique(y_vals))
  
  # Initialise empty matrices for values and text labels
  mat_values <- matrix(NA, nrow = length(x_labels), ncol = length(y_labels),
                       dimnames = list(x_labels, y_labels))
  mat_text   <- matrix(NA, nrow = length(x_labels), ncol = length(y_labels),
                       dimnames = list(x_labels, y_labels))
  
  # Populate matrices using a loop
  for(i in 1:nrow(df_heatmap)) {
    r_idx <- as.character(x_vals[i])
    c_idx <- as.character(y_vals[i])
    mat_values[r_idx, c_idx] <- x_vals[i]
    mat_text[r_idx, c_idx]   <- as.numeric(text_vals[i])
  }
  
  # Adjust plot margins to ensure names fit on the left side
  par(mar = c(5, 7, 4, 2))
  
  # Define a color palette (e.g., shades of blue)
  color_palette <- hcl.colors(12, "Blues", rev = FALSE)
  
  # Draw the heatmap background using the 'number' matrix
  image(
    x = 1:length(x_labels),
    y = 1:length(y_labels),
    z = t(mat_values),
    xaxt = "n",             # Suppress default X-axis
    yaxt = "n",             # Suppress default Y-axis
    xlab = xlab,
    ylab = ylab,
    main = title,
    bty="l",
    col = "white"#color_palette
  )
  
  # Draw the background grid squares using the custom percentage-based color matrix
  
  num_colors <- 100
  color_palette <- heat.colors(num_colors, rev=TRUE)
  
  color_index_matrix <- matrix(
    as.numeric(cut(mat_text, breaks = num_colors, labels = FALSE)), 
    nrow = nrow(mat_text)
  )
  
  # Add custom axis labels
  axis(1, at = 1:length(x_labels), labels = x_labels)
  axis(2, at = 1:length(y_labels), labels = y_labels, las = 2)
  
  # Overlay the independent percentage text onto the cells
  for(x in 1:length(x_labels)) {
    for(y in 1:length(y_labels)) {
      
      
      # Extract the pre-calculated global color index for this specific cell
      col_idx <- color_index_matrix[x, y]
      
      # If the index is not NA, color the background tile
      if(!is.na(col_idx)) {
        rect(
          xleft   = x - 0.5, ybottom = y - 0.5, 
          xright  = x + 0.5, ytop    = y + 0.5, 
          col     = color_palette[col_idx], 
          border  = "white"
        )
      }
      
      text_val <- as.character(mat_text[x, y])
      # Only print text if a data point exists in that cell
      if (!is.na(text_val) && text_val != "") {
        text(x, y, labels = text_val, col = "black", font = 2)
      }
    }
  }
  
}

# base r heatmap example
df_heatmap <- df_abx
heatmapper(df_heatmap$contrib_txt,df_heatmap$pick,df_heatmap$pct,"Rank","Team","Title")


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