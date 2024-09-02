library(dplyr)
library(maps)
library(readxl)
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(sf)

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
  #geom_label_repel(box.padding = 0.5) +
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

################################################################################
########################## region map plot ######################################

# this example takes the regions/boundaries as provided by the shape file

# rather than plot using co-ordinates, this uses the defined shapes, and colours the areas of the shapes by e.g. count/number
# to get this, join on data as needed

# load the boundary shape folder name
file <- "Regions_December_2023_Boundaries_EN_BSC_332837636992714114" # extract zipped file

# using the sf library, read the shape files
eng_reg_map <- st_read(paste(folder_location,file,sep=""))

# at this point, from the postcode data use an intermediate file to join the postcodes to region locations
# the postcode lookup file
file <- "pcd_oa_lsoa_msoa_ltla_utla_rgn_ctry_ew_may_2021_lu_v2.csv"
postcode_lookup <- read.csv(paste(folder_location,file,sep=""))

# select something like RGN22CD for the grid to match on
postcode_lookup1 <- select(postcode_lookup, pcd, rgn22cd, rgn22nm) # grid number and region name

# take the postcode from the original source data and get the match
df_post1 <- left_join(df_post,postcode_lookup1, join_by(postcode==pcd))

# then get count by region
count_region <- df_post1 %>%
  group_by(rgn22nm) %>%
  summarise(pct=pct)#n()

# on eng_reg_map left_join this data
eng_reg_map <- left_join(eng_reg_map, count_region, join_by("RGN23NM"==rgn22nm))

# fill count na with 0
eng_reg_map[is.na(eng_reg_map)] = 0

# graph this
ggplot(eng_reg_map, aes(fill=pct))+
  geom_sf(colour = "grey")+
  theme_void()+
  labs(title="UK map of train station locations, with % of station contribution to total")+
  theme(plot.title = element_text(colour = "black"))+
  #theme(legend.position = 'none') + 
  theme(title = element_text(size = 9))+
  theme(plot.background = element_rect(fill = "white"))

ggsave(paste(output_folder,"test_regions_ggrepel.png",sep=""),width=22,height=12,units="cm")

################################################################################
########################## UTLA map plot ######################################

# this example takes the regions/boundaries as provided by the shape file

# rather than plot using co-ordinates, this uses the defined shapes, and colours the areas of the shapes by e.g. count/number
# to get this, join on data as needed

# load the boundary shape folder name
file <- "Upper_Tier_Local_Authorities_December_2022_Boundaries_UK_BFC_-745140168582188301" # extract zipped file

# using the sf library, read the shape files
eng_utla_map <- st_read(paste(folder_location,file,sep=""))

# at this point, from the postcode data use an intermediate file to join the postcodes to region locations
# the postcode lookup file
#file <- "pcd_oa_lsoa_msoa_ltla_utla_rgn_ctry_ew_may_2021_lu_v2.csv"
#postcode_lookup <- read.csv(paste(folder_location,file,sep=""))

# select something like RGN22CD for the grid to match on
postcode_lookup2 <- select(postcode_lookup, pcd, utla22cd, utla22nm) # grid number and region name

# take the postcode from the original source data and get the match
df_post2 <- left_join(df_post,postcode_lookup, join_by(postcode==pcd))

# then get count by region
count_utla <- df_post2 %>%
  group_by(utla22nm) %>%
  summarise(pct=pct)#n()

# on eng_reg_map left_join this data
eng_utla_map <- left_join(eng_utla_map, count_utla, join_by("UTLA22NM"==utla22nm))

# fill count na with 0
eng_utla_map[is.na(eng_utla_map)] = 0

# graph this
ggplot(eng_utla_map, aes(fill=pct))+
  geom_sf(colour = "grey")+
  theme_void()+
  labs(title="UK map of train station locations, with % of station contribution to total")+
  theme(plot.title = element_text(colour = "black"))+
  #theme(legend.position = 'none') + 
  theme(title = element_text(size = 9))+
  theme(plot.background = element_rect(fill = "white"))

ggsave(paste(output_folder,"test_utla_ggrepel.png",sep=""),width=22,height=12,units="cm")

end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)