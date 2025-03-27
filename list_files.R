library(dplyr)

folder <- r"[C:\Users\kelvi\Desktop]"

files <- list.files(folder, full.names = TRUE, recursive = TRUE) # recursive for all sub-folders

df <- data.frame(folder=gsub("[^/]*$","",files), file=gsub("(?=^||\\/).*(\\/)","",files, perl=TRUE), file_link=files, file_ext = gsub(".*(?<=\\.)","",files, perl=TRUE))

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

# write output .csv
write.csv(df, paste(folder,"\\","files_and_folders.csv", sep=""), row.names=FALSE)

# group by
df_yr <- df |>
  group_by(mod_yr) |>
  summarise(count=n())

# filetypes
df_f <- df |>
  group_by(file_ext) |>
  summarise(count=n())

df_f <- df_f[order(-df_f$count),]

# top 10 plot
plot.new()
plot(x=reorder(as.factor(df_f$file_ext[1:10]),-df_f$count[1:10]), y=df_f$count[1:10], type="p",
xlab="File type",
ylab="Count",
main=" File types by count")

