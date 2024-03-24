library(dplyr)
library(ggplot2)

data <- c(1,2,3,4,5,6,7,8,9,10)
typer <- c("a","b","a","b","a","b","a","b","a","b")

# make a dataframe
df <- data.frame(data=data, type=typer)

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

# rename specific column
colnames(df)[colnames(df) == "pct_v"] <- "pct_t"

# get top 10
df_top10 <- df[1:10,]

# make another, unused dataframe and join the first dataframe onto this second dataframe
df_unused <- data.frame(col1=data,col2=c(50,50,50,50,50,50,50,50,50,50))

df_unused <- left_join(df_unused, df, by=c("col1"="data"))

################################################################################

# bar graph
#savename of graph title
grptt=paste("title",sep="")

#create line graph
ggplot(df,aes(x=factor(data),y=`data`, fill=`type`))+
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
ggplot(df,aes(x=`row_nums_same`,y=`c_number`, group=`type`, color=`type`))+
geom_line()+
#scale_x_date(date_labels="%d/%m/%Y",date_breaks="1 week")+ #can be 1 day, 1 week, 1 month
#geom_text(data=filter(df,`c_number`==last(`c_number`)), aes(label=round(`c_number`,0)),vjust=-0.5)+
geom_text(data=data_ends,aes(label=round(`c_number`,0)),vjust=-0.5,size=2.5)+
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