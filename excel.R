library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)

############################# configuration ###################################
output_folder <- "C:\\Users\\kelvi\\Desktop\\"
excel_file_name <- "name.xlsx"

############################ make df ##########################################
data <- c(1,2,3,4,5,6,7,8,9,10)
typer <- c("a","b","a","b","a","b","a","b","a","b")

# make a dataframe
df <- data.frame(data=data, type=typer)

########################### make graph ########################################
graph <- ggplot(df, aes(type,data))+
  geom_line()+
  geom_text(aes(label=data))+
  labs(x="a",y="b",title="c")

ggsave(paste(output_folder,"file.png",sep=""),height=15,width=30,units="cm")

########################## write to excel #####################################

# simple - write as xlsx
#write.xlsx(df,
#           sheetName = "some_sheet",
#           file = "out_file.xlsx")

# more advanced - detailed
# https://cran.r-project.org/web/packages/openxlsx/vignettes/Formatting.html
#options(openxlsx.borderStyle = "thin")
#options(openxlsx.borderColour = "#4F81BD")

color_cells <- function(wb, sheet, rows, cols, color) {
  cellStyle <- createStyle(fgFill = color)
  addStyle(wb, sheet = sheet, cellStyle, rows = rows, cols = cols, gridExpand = TRUE)
  wb
}

wb <- createWorkbook()
addWorksheet(wb, "some_sheet")
# blank formatting - optional
for (i in 1:100){
  for (j in 1:100){
    color_cells(wb, "some_sheet", rows = i, cols = j, color = "white")
  }
}
insertImage(wb, "some_sheet", paste(output_folder,"file.png",sep=""), width=30, height=15, startRow=10, startCol=10, units="cm")
#writeData(wb,"some_sheet",`df`,startRow=1,startCol=1,borders = "rows", borderStyle = "medium")
writeDataTable(wb,"some_sheet",`df`,startRow=1,startCol=1,tableStyle="TableStyleMedium1")

# freeze panes
freezePane(wb, "some_sheet", firstActiveRow = 2, firstActiveCol = 1)

saveWorkbook(wb,file=paste(output_folder,excel_file_name,sep=""),overwrite=TRUE)

# plot graph on worksheet
#wb <- createWorkbook()

#addWorksheet(wb, "Armiaplt")
#insertImage(wb, "Armiaplt", "filearmia.png", width=30, height=15, units="cm")
#writeData(wb,"Armia",`mydf1`,startRow=1,startCol=1)

#saveWorkbook(wb,file="name.xlsx",overwrite=TRUE)

######################## remove locally stored images #########################
file.remove(paste(output_folder,"file.png",sep=""))