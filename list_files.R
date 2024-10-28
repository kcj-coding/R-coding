folder <- r"[C:\Users\janan\Desktop]"

files <- list.files(folder, full.names = TRUE, recursive = TRUE) # recursive for all sub-folders

df <- data.frame(folder=gsub("[^/]*$","",files), file=gsub("(?=^||\\/).*(\\/)","",files, perl=TRUE), file_link=files)

#file_mod <- fs::file_info(paste(df$file_link))

df$lst_modified <- fs::file_info(paste(df$file_link))$modification_time
df$access <- fs::file_info(paste(df$file_link))$access_time
df$change <- fs::file_info(paste(df$file_link))$change_time
df$creation <- fs::file_info(paste(df$file_link))$birth_time

df$size <- fs::file_info(paste(df$file_link))$size

# get year and month-year of modification
df <- df |>
  mutate(mod_yr=as.numeric(format(df$lst_modified, "%Y")),
         mnthyr=format(df$lst_modified, "%b-%Y"),
         uk_date=format(df$lst_modified, "%d/%b/%Y"))

# group by
df_yr <- df |>
  group_by(mod_yr) |>
  summarise(count=n())

