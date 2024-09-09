library(dplyr)
library(stringr)
library(ggplot2)

output_folder <- "C:\\Users\\kelvi\\Desktop\\"

dat <- rep(rnorm(90),3)
vals <- rep(c("A","B","C"),90)

df <- data.frame(data=dat, vals=vals)
df$dat <- df$dat * 1e2
df$xdata <- seq.int(nrow(df))

#for multiple series need to filter dataframe by series type, and collate the last value
#filter for last value
data_ends <- df %>%
  group_by(`vals`) %>%
  top_n(1,`dat`) # this can also show same values elsewhere in data

# for the graphs, credit goes to r-graph-gallery

################################################################################

# clear line graph

grptt = "Title of graph"

ggplot(df,aes(x=`xdata`,y=`dat`,group=vals,color=vals))+
  geom_line()+
  geom_point()+
  #scale_color_manual(values=pal)+
  geom_text(data=data_ends,aes(label=paste(round(`dat`,0),"",sep="")),vjust=-0.5,size=2,color="black")+
  
  geom_text(x=265,y=15,label="End of data",size=2,color="black")+
  annotate("segment", x = 265, xend = 270, y = 15, yend = 10, colour = "black", size=0.1,
           #alpha=1,
           arrow=arrow(length = unit(.2,"cm")))+
  theme_minimal()+
  theme(plot.title = element_text(
    size = 14,
    #face = "bold",
    hjust = .5,
    margin = margin(10, 0, 30, 0)
  ))+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9, color = "black"),
        legend.box.margin = margin(t = 30),
        legend.title=element_blank(),
        legend.background = element_rect(
          color = "white", 
          size = .3, 
          fill = "white"
        ),
        legend.key.height = unit(.25, "lines"),
        legend.key.width = unit(2.5, "lines"))+
  theme(axis.text.x=element_text(angle=0,size=6))+
  #geom_vline(xintercept=2001,linetype=4)+
  #geom_vline(xintercept=2008,linetype=4)+
  theme(panel.grid.major.x=element_line(size=.1,color="black", linetype="dashed"),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(), # or solid, dotted
        panel.grid.minor.y=element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_blank(),
  )+
  labs(x="",y="",title=grptt)


#savename of graphs
svname=paste("name1",".png",sep="")

#save graph
ggsave(paste(output_folder,svname,sep=""),height=15,width=30,units="cm")

################################################################################

# circular bar chart

# need to have totals
df1 <- df |>
  group_by(vals) |>
  summarise(dat = sum(dat))

df1$xdata <- seq.int(nrow(df1))

plt <- ggplot(df1) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:1) * max(df1$dat)),
    color = "lightgrey"
  ) + 
  geom_col(
    aes(
      x = reorder(str_wrap(df1$vals, 5), df1$dat),
      y = df1$dat,
      fill = "#CD2463"
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  scale_fill_manual(values=c("#CD2463")) +
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(df1$vals, 5),df1$dat),
      y = df1$dat
    ),
    size = 3,
    color = "gray12"
  ) +
  geom_text(
    aes(
      x = reorder(str_wrap(df1$vals, 5),df1$dat),
      y = df1$dat,
      label = df1$dat
    ),
    size = 3,
    color = "gray12",
    #vjust=-0.5,
    hjust=-1.0
  ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(df1$vals, 5), df1$dat),
      y = 0,
      xend = reorder(str_wrap(df1$vals, 5), df1$dat),
      yend = max(df1$dat)
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  
  # Make it circular!
  coord_polar()


plt <- plt +
  
# Scale y axis so bars don't start in the center
scale_y_continuous(
  limits = c(-15, max(df1$dat)*1.1),
  expand = c(0, 0),
  breaks = c(0, max(df1$dat))
) + 

theme(
  # Remove axis ticks and text
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_blank(),
  # Use gray text for the region names
  axis.text.x = element_text(color = "gray12", size = 8),
  # Move the legend to the bottom
  legend.position = "none",
)


plt <- plt + 
  # Add labels
  labs(
    title = grptt,
    #  #  subtitle = ""
    # ),
    caption = "") +
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 14, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )

plt

ggsave(paste(output_folder,"circularbars.png",sep=""),width=22,height=12,units="cm")
