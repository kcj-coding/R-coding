library(dplyr)
library(ggplot2)

# want to filter df by column names and values specified by user

gc()

output_folder <- "C:\\Users\\kelvi\\Desktop\\"

# check output folder exists
if (!file.exists(paste(output_folder,sep=""))){
  dir.create(file.path(paste(output_folder,sep="")), recursive=TRUE) # recursive to also build any sub-folders
}

# create df
dat <- rep(rnorm(90),3)
ids <- rep(c("A","B","C"),times=90)
nums <- rep(c(2018,2019), each=135) # keep the order
extra <- rep(rnorm(90),3)

df <- data.frame(data=dat, ids=ids, nums=nums, extra=extra)

################################################################################

# function for df creation
df_create <- function(data, group_data, xdata, spec){
  
  dff <- data |>
    group_by(across(all_of(group_data))) |> #, na.rm = TRUE) |>
    summarise(total=spec(data))#, na.rm = TRUE), .groups="drop")
  
  # get IQR and filter away when small
  dff <- dff |>
    group_by(across(xdata)) |>
    mutate(IQR = IQR(total)) |>
    ungroup() |>
    filter(IQR >= 1)
  
  return (dff)
}

# function for graph creation
boxplot_graph <- function(data, xval, xlab, title){
  
  ggplot(data, aes(x=!!as.symbol(xval), y=total))+ #!!as.symbol() for turning character column name into actionable variable name
    geom_boxplot(outlier.shape = NA)+ # remove outliers when jitter
    geom_jitter(height = 0, width = 0.0)+
    geom_point()+
    theme_classic()+
    labs(y="Total", x=xlab, title=title)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

################################################################################

# run for loop of functions

# get everything by type
df_total <- df_create(df, c("nums", "ids", "extra"), "ids", sum)

# graph the data
boxplot_graph(df_total, "ids", "IDs", "Title")
ggsave(paste(output_folder,"total.png",sep=""), width=30, height=15, units="cm", type="ragg", dpi=128)

# for each nums and each type, get df and graph
unq_type <- unique(df$ids)
unq_nums <- unique(df$nums)

for (i in unq_type){
  for(j in unq_nums){
    
    # filter the data
    df_test <- df |>
      filter(ids == i) |>
      filter(nums == j)
    
    df_test1 <- df_create(df_test, c("nums", "extra","ids"), "ids", sum)
    
    # graph the data
    boxplot_graph(df_test1, "ids", "IDs", paste("Type: ",i," by ", j, sep=""))
    ggsave(paste(output_folder,i,j,".png",sep=""), width=30, height=15, units="cm", type="ragg", dpi=128)
  }
}