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

# conditional format the data based on values
# https://ycphs.github.io/openxlsx/articles/Formatting.html

negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

conditionalFormatting(wb, "some_sheet", cols=1, rows=1:11, rule="<=5", style = negStyle)
conditionalFormatting(wb, "some_sheet", cols=1, rows=1:11, rule=">=0", style = posStyle)

# format the cells below the datatable
color_cells(wb, "some_sheet", rows = nrow(df)+1:nrow(df)+1 , cols = ncol(df)+1:ncol(df)+1, color = "blue")

setColWidths(wb, "some_sheet", cols = seq_len(ncol(df)), widths = 1.07) # or "auto")
setRowHeights(wb, "some_sheet", rows = seq_len(nrow(df)), heights = 7.5)

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

################################################################################

################ load an excel workbook which has formatting and maintain the formatting 

# can use a function iwth openxlsx or use openxlsx2

# https://stackoverflow.com/questions/11228942/write-from-r-into-template-in-excel-while-preserving-formatting

# other info https://stackoverflow.com/questions/75934468/pasting-and-writing-data-onto-existing-excel-file-using-r

# using openxlsx2
library(openxlsx2)

# load template file
template_file <- wb_load(paste(output_folder,"theme.xlsx",sep=""), sheet="Sheet1") |>
  wb_add_data(x = df, sheet="Sheet1") |>
  wb_save(file=paste(output_folder,"saved_file.xlsx",sep=""))

# using openxlsx

# use function below to copy styling
copyStyle <- function(from_wb, to_wb, from_sheet, to_sheet) {
  # check for workbook objects
  if (!(inherits(from_wb, "Workbook") && inherits(to_wb, "Workbook"))) { 
    stop("from_wb and to_wb must be Workbook objects.")
  }
  
  # get all sheet names from workbooks
  from_sheets <- from_wb$sheet_names
  to_sheets <- to_wb$sheet_names
  
  # convert sheets from numeric to sheet name. wb$styleObjects uses sheet name
  if (is.numeric(from_sheet)) {
    from_sheet <- from_wb$getSheetName(from_sheet)
  }
  
  if (is.numeric(to_sheet)) { 
    to_sheet <- to_wb$getSheetName(to_sheet)
  }
  
  # if sheet name given check that it exists
  if (is.character(from_sheet) && !from_sheet %in% from_sheets) {
    stop(glue::glue("{from_sheet} was not found in from_wb"))
  }
  
  if (is.character(to_sheet) && !to_sheet %in% to_sheets) {
    stop(glue::glue("{to_sheet} was not found in to_wb"))
  }
  
  # get from_wb sheet styles
  from_styles <- purrr::keep(from_wb$styleObjects, ~ .x$sheet == from_sheet)
  
  # add styles to to_wb
  purrr::walk(from_styles, ~ openxlsx::addStyle(to_wb, 
                                                to_sheet, 
                                                .x$style, 
                                                rows = .x$rows, 
                                                cols = .x$cols))
  return(to_wb)
}

# load workbook
template_file1 <- loadWorkbook(paste(output_folder,"theme.xlsx",sep=""))

# create workbook to save
wb1 <- createWorkbook()
addWorksheet(wb1, "some_sheet")

# apply styling to this workbook
writeData(wb1,"some_sheet",`df`,startRow=1,startCol=1)

copyStyle(from_wb = template_file1, to_wb = wb1, from_sheet = "Sheet1", to_sheet = 1)

# save the workbook
saveWorkbook(wb1,file=paste(output_folder,"saved_file1.xlsx",sep=""),overwrite=TRUE)

#################### or, more easily
# load workbook
template_file1 <- loadWorkbook(paste(output_folder,"theme.xlsx",sep=""))

# add df to this already formatted workbook
writeData(template_file1,"Sheet1",`df`,startRow=1,startCol=1) # name of worksheet in template_file1

# save the workbook with a new name
saveWorkbook(wb1,file=paste(output_folder,"saved_file11.xlsx",sep=""),overwrite=TRUE)