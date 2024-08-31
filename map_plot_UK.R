library(dplyr)
library(maps)
library(readxl)
library(openxlsx)
library(ggplot2)
library(ggrepel)

start_time <- Sys.time()

# get statistics csv file from https://geoportal.statistics.gov.uk/datasets/
# search for postcodes NSPL national statistics postcodes lookup (csv file)

folder_location <- "C:\\Users\\kelvi\\Desktop\\"
output_folder <- "C:\\Users\\kelvi\\Desktop\\"

# load nspl file
postcodes <- read.csv(paste(folder_location,"NSPL_AUG_2024_UK.csv",sep=""))

postcodes <- select(postcodes, pcd, lat, long)

# load a file with postcodes or create a df with postcodes
df_post <- data.frame(place=c("Waterloo","Piccadilly"),postcode=c("SE1 7ND","M60 7RA"))

df_post$number <- 1

#get % contribution of place to total
df_post <- df_post %>%
  mutate(total=sum(number)) %>%
  group_by(place) %>%
  mutate(pct=(number/total)*100)

# get the lat and long from these postcodes added to the df_post
df_post <- left_join(df_post, postcodes, join_by(postcode==pcd))

# plot these on a UK map

worldmap = map_data('world')

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = df_post, 
             aes(x = as.numeric(long), 
                 y = as.numeric(lat), size = number, color = number), alpha = .7) + 
  geom_text(data = df_post,aes(x = as.numeric(long), 
                                 y = as.numeric(lat),label=place),vjust=2.2, size=2.5)+
  geom_text(data = df_post,aes(x = as.numeric(long), 
                                 y = as.numeric(lat),label=paste(pct,"%",sep="")),vjust=5.3, size=2.0)+
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  labs(title="UK map of train station locations, with % of station contribution to total")+
  theme(plot.title = element_text(colour = "black"))+
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 9))+
  theme(plot.background = element_rect(fill = "white"))

ggsave(paste(output_folder,"test.png",sep=""),width=22,height=12,units="cm")

# plot these on a UK map, ggrepel

worldmap = map_data('world')

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = df_post, 
             aes(x = as.numeric(long), 
                 y = as.numeric(lat), size = number, color = number), alpha = .7) + 
  geom_label_repel(box.padding = 0.5) +
  #geom_text(data = df_post,aes(x = as.numeric(long), 
  #                             y = as.numeric(lat),label=place),vjust=2.2, size=2.5)+
  #geom_text(data = df_post,aes(x = as.numeric(long), 
  #                             y = as.numeric(lat),label=paste(pct,"%",sep="")),vjust=5.3, size=2.0)+
  geom_label_repel(data = df_post,aes(x = as.numeric(long), 
                                        y = as.numeric(lat),label=paste(place,"\n",pct,"%",sep="")),fill = "white",# xlim = c(-120,160), ylim = c(-35, 59),
                   min.segment.length = 0,box.padding=0.1, max.time = 1, max.iter = 1e5, max.overlaps=10, size=1.5)+
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  labs(title="UK map of train station locations, with % of station contribution to total")+
  theme(plot.title = element_text(colour = "black"))+
  theme(legend.position = 'none') + 
  theme(title = element_text(size = 9))+
  theme(plot.background = element_rect(fill = "white"))

ggsave(paste(output_folder,"test_ggrepel.png",sep=""),width=22,height=12,units="cm")

end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)