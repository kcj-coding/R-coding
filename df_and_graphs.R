#library(tidyverse)
library(dplyr)
library(summarytools)
library(ggplot2)
library(patchwork)



data <- c(1,2,3,4,5,6,7,8,9,10)
typer <- c("a","b","a","b","a","b","a","b","a","b")

# make a dataframe
df <- data.frame(data=data, type=typer)

# apply a function to the dataframe
test <- function(x,y){
  if(x > y){
    #val <- "yes"
    "yes"
  }
  
  else{
    #val <- "no"
    "no"
  }
  #return (val)
}

df$zz <- mapply(test, df$data, lead(df$data,1,0)) # apply a function and use specified arguments in function

# get summary
df_summary = as.data.frame(summary(df))

# display summary data graphically
view(summarytools::dfSummary(df))

# histogram of numeric values
df_hist <- df[, sapply(df, class) == 'numeric']
hist(unlist(df_hist))

# plot all numeric data on histograms
library(tidyr)
if (!is.null(ncol(df_hist))){
  hists <- df_hist |>
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "value")
  
  ggplot(hists, aes(x=value))+
    geom_histogram()+
    facet_wrap(~variable, scales="free", ncol=4)+
    theme_classic()
}

# add a column for the number of instances
df$number <- 1
df$row_num <- as.numeric(rownames(df))

# get cumulative number by type
df <- df %>%
  group_by(`type`) %>%
  mutate(`c_number`= cumsum(`data`)) %>%
  ungroup

# pct calc by type
df <- df %>%
  group_by(type) %>%
  mutate(pct_v = round((data/sum(data)*100),2)) %>%
  ungroup

# group data by type of data e.g. get all numeric columns
df_numeric <- df %>%
  group_by(`data`) %>%
  summarise(across(where(is.numeric),sum))

# remove any duplicates... column selection $ and fromLast optional
df <- df[!duplicated(df$data, fromLast = TRUE), ]

# order df by data column
df <- df[order(df$data, decreasing = FALSE), ]

# rank by col1
df$rank <- NA # initialise as NA
df$rank[order(-df$col1)] <- 1:nrow(df$col1)

# rename specific column
colnames(df)[colnames(df) == "pct_v"] <- "pct_t"

# query loop based on column name
col_names <- c("col1")
for (i in col_names){
  
  new_name <- paste(i,"_fct",sep="")
  
  df1 <- df |>
  mutate(!!i := as.numeric(!!as.name(i)),
         !!new_name := as.factor(!!as.name(i)))
}

# new column with previous values, lag for prv, lead for next
df$prv_data <- lag(df$data, n=1, default=0) # default for setting any NA or missing values


# get top 10
df_top10 <- df[1:10,]

# filter type to examples containing a

# filter using subset - faster but more difficult to read
df_a <- df[grepl("a",df$type),]

# filter using dplyr and filter - easier to read but slower than subsetting
df_a <- df %>%
  filter(grepl("a",df$type))

# note, for grepl searching for pattern within x, "a" within a column is fine but you cannot search column within a column, instead you would need to iterate through the rows
d <- list()
for (i in nrow(df_a)){
  df_chk <- df_a[,i]

  datad <- df_chk[grepl(substr(df_chk$type,1,1), df_chk$type),]

  d[[i]] <- datad
  
}

dfs <- do.call(rbind,d)

# make another, unused dataframe and join the first dataframe onto this second dataframe
df_unused <- data.frame(col1=data,col2=c(50,50,50,50,50,50,50,50,50,50))

df_unused <- left_join(df_unused, df, by=c("col1"="data"))

# countif function like excel
vector_count <- function(df1,df2){
  store <- vector()
  counts_nbr <- vector()
  items <- unique(df1)
  for (i in 1:length(items)){
    i <- df1[i]
    cnt <- sum(df2 == i)
    
    store[i] <- i
    counts_nbr[i] <- cnt
  }
  
  df <- data.frame(item=store, counts=counts_nbr)
  return(df)
}

# count df_unused col1 in df typer
df_count <- vector_count(df_unused$col1, df$typer)

################################################################################

# bar graph
#savename of graph title
grptt=paste("title",sep="")

#create line graph
bg <- ggplot(df,aes(x=factor(data),y=`data`, fill=`type`))+
  geom_bar(aes(fill=`type`),position="dodge",stat="identity",width=0.5)+
  #scale_x_date(date_labels="%d/%m/%Y",date_breaks="1 week")+ #can be 1 day, 1 week, 1 month
  geom_text(aes(label=round(`data`,0)),position=position_dodge(width=.9),vjust=-0.5,size=1.5)+
  scale_fill_manual(values=alpha(c("#CD2456","#14022E")),name="xyz")+
  #geom_point(aes(shape=`column`))+
  #geom_vline(aes(xintercept=as.numeric(Date[c(90)])))+
  #geom_vline(aes(xintercept=as.integer(as.POSIXct("2020-05-05"))),linetype=4)+
  theme_classic()+
  #ylim(0,max((`yaxis`)+5))+
  #facet_grid(~col)+
  #coord_flip()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=0,size=10))+
  labs(x="Instance",y="Data",title=grptt)


#savename of graphs
#svname=paste("name",".png",sep="")

#save graph
#ggsave(path="C:\\",svname,height=15,width=30,units="cm")

bg
################################################################################

# line graph
#savename of graph title
grptt=paste("title",sep="")

# make row_nums the same
df$row_nums_same <- ifelse(df$type=="a",df$row_num+1,df$row_num)

#for multiple series need to filter dataframe by series type, and collate the last value
#filter for last value
data_ends <- df %>%
  group_by(`type`) %>%
  top_n(1,`c_number`)

#then use this in geom_text:
#geom_text(data=data_ends,aes(label=round(`yvalue`,0)),vjust=-0.5,size=2.5)+

#create line graph
ln <- ggplot(df,aes(x=`row_nums_same`,y=`c_number`, group=`type`, color=`type`))+
  #ggplot(df,aes(x=`row_nums_same`,y=`c_number`, group=1, color=1))+  # single line only
  geom_line()+
  #scale_x_date(date_labels="%d/%m/%Y",date_breaks="1 week")+ #can be 1 day, 1 week, 1 month
  #geom_text(data=filter(df,`c_number`==last(`c_number`)), aes(label=round(`c_number`,0)),vjust=-0.5)+
  geom_text(data=data_ends,aes(label=round(`c_number`,0)),vjust=-0.5,size=2.5)+
  #geom_text(data=filter(df_c,tot == last(tot)), aes(label=round(`tot`,0)),vjust=-0.5,size=2.5)+ # single line only
  scale_color_manual(values=alpha(c("#CD2456","#14022E")),name="xyz")+
  #scale_x_continuous(1:5)+
  #geom_point(aes(shape=`column`))+
  #geom_vline(aes(xintercept=as.numeric(Date[c(90)])))+
  #geom_vline(aes(xintercept=as.integer(as.POSIXct("2020-05-05"))),linetype=4)+
  theme_classic()+
  ylim(0,max((df$`c_number`)+5))+
  #facet_grid(~type)+
  #coord_flip()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=0,size=10))+
  labs(x="Instance",y="Cumulative",title=grptt)


#savename of graphs
#svname=paste("name",".png",sep="")

#save graph
#ggsave(path="C:\\",svname,height=15,width=30,units="cm")

ln

#################### patchwork to combine graphs ###############################

(ln+bg)
(ln/bg)

################################################################################
# line graph with only 1 line coloured
# https://stackoverflow.com/questions/68079185/how-to-make-one-line-color-in-geom-line-to-overshadow-other-line-colors
# https://stackoverflow.com/questions/72058812/highlighting-lines-and-gray-out-rest-in-multiple-line-chart-with-ggplot2

for (i in unique(df$type)){
  
  #filter for last value
  data_ends <- df %>%
    filter(type == i) %>%
    group_by(`type`) %>%
    top_n(1,`c_number`)
  
  #savename of graph title
  grptt=paste("title",sep="")
  
  # set colours of lines
  #line_clr <- c("#CD2456", "#CECECE")
  
  #create line graph
  ggplot(df,aes(x=`row_nums_same`,y=`c_number`, group=`type`, color=ifelse(type==i, "#CD2456","#CECECE")))+#`type`))+
    #ggplot(df,aes(x=`row_nums_same`,y=`c_number`, group=1, color=1))+  # single line only
    geom_line(aes(group=type, color=ifelse(type==i, "#CD2456","#CECECE")))+#`type`))+
    # add line of interest
    #geom_line(data=df_alt[df_alt$type == i,], aes(group=type, color=type))+ # filter line to colour as line of interest
    #scale_x_date(date_labels="%d/%m/%Y",date_breaks="1 week")+ #can be 1 day, 1 week, 1 month
    #geom_text(data=filter(df,`c_number`==last(`c_number`)), aes(label=round(`c_number`,0)),vjust=-0.5)+
    geom_text(data=data_ends,aes(label=round(`c_number`,0)),vjust=-0.5,size=2.5)+
    #geom_text(data=filter(df_c,tot == last(tot)), aes(label=round(`tot`,0)),vjust=-0.5,size=2.5)+ # single line only
    #scale_color_manual(values=alpha(c("#CD2456","#CECECE")),name="xyz")+
    
    scale_color_manual(values = c(rep(c("#CD2456"), each = 1),
                                  rep("#CECECE", nrow(df) - 1)))+
    
    #scale_x_continuous(1:5)+
    #geom_point(aes(shape=`column`))+
    #geom_vline(aes(xintercept=as.numeric(Date[c(90)])))+
    #geom_vline(aes(xintercept=as.integer(as.POSIXct("2020-05-05"))),linetype=4)+
    theme_classic()+
    ylim(0,max((df$`c_number`)+5))+
    #facet_grid(~type)+
    #coord_flip()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=0,size=10))+
    labs(x="Instance",y="Cumulative",title=paste(grptt,i, sep=" "))
  
  
  #savename of graphs
  svname=paste("name",i,".png",sep="")
  
  #save graph
  #ggsave(path="C:\\Users\\kelvi\\Desktop\\",svname,height=15,width=30,units="cm")
}

################################################################################

# make a heatmap
ggplot(df,aes(x=`row_num`,y=`row_num`))+
  geom_tile(aes(fill=`c_number`))+
  geom_text(data=df,aes(label=round(`c_number`,0)),vjust=0.25,size=2.5)+
  #scale_x_continuous(1:10)+
  #scale_y_continuous(1:10)+
  scale_fill_distiller(palette = "RdYlBu")+
  theme_classic()+
  labs(x="Instance",y="Instance",title="title")

#savename of graphs
#svname=paste("name",".png",sep="")

#save graph
#ggsave(path="C:\\",svname,height=15,width=30,units="cm")

################################################################################

# get boxplot of data and list the values

ggplot(df,aes(x=type,y=data))+
  geom_boxplot(outlier.shape = NA)+ # remove outliers when jitter
  #geom_jitter(height = 0, width = 0.0)+
  geom_point() +
  theme_classic()+
  labs(y="data",x="type")

stats <- boxplot(data ~ type, data=df)
stats

####################### dates ##################################################
# create dates from 25th february to 10th march in year 2023
date <- as.Date("2023-02-24")#+5  for next 5 days # make one value before what is wanted
# or can do 
# date <- seq.Date(as.Date("2023-02-25"), length=2, by='5 days' ) # or by '3 months', '-1 year') etc

end_date <- as.Date("2023-03-10")

t <- difftime(end_date, date, units = c("days"))

# make df of dates
# make list of dates between these values
dater <- data.frame(date=NULL)
for (i in 1:difftime(end_date ,date , units = c("days"))){
  datei <- data_frame(date=date+i)
  dater <- rbind(dater,datei)
}

# filter to dates greater than the 3rd of march
dater_filtered <- dater %>%
  filter(date >= "2023-03-03")

####################### times ##################################################
# or can make df of times
# difftime can do years, months, days, hours, minutes, secs

# make a df of time between 00:00 and 23:00 (every hour)
#start_time <- as.POSIXct("00:00")

# https://stackoverflow.com/questions/12649641/calculating-time-difference-in-r
# https://stackoverflow.com/questions/11666172/calculating-number-of-days-between-2-columns-of-dates-in-data-frame
# https://stackoverflow.com/questions/6871016/adding-days-to-a-date-in-python  
# https://stackoverflow.com/questions/10322035/r-adding-days-to-a-date

####################### datetime ##################################################
start <- as.POSIXct("2023-02-24 00:00:00")
# add 3 hours
end <- start + 3*60*60

# make df of these entries
hoursr <- data.frame(date=NULL)
for (i in 1:difftime(end ,start , units = c("hours"))){
  hoursi <- data_frame(date=start+(i*60*60))
  hoursr <- rbind(hoursr,hoursi)
}

##################################################################################

# datetime graphs
dater$num <- 1

# graph of dates with 3 days separation
ggplot(dater,aes(date,num))+
  geom_point()+
  scale_x_date(date_labels="%d-%m",date_breaks  ="3 days") # days, months, years