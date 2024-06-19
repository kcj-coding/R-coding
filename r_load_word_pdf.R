library(officer)
library(pdftools)
library(dplyr)
library(tidytext)
library(stringr)

# specify folder location
folder_location <- "C:\\Folder"

# specify file types
list_word_files <- list.files(folder_location, pattern="docx|doc") # docx or doc files
list_pdf_files <- list.files(folder_location, pattern="pdf")

# make initial df for word docs
word_df <- data.frame()

for (file in list_word_files){
  text_df <- paste0(folder_location, "/", file) %>%
    officer::read_docx() %>%
    officer::docx_summary() %>%
    mutate(docname = file) %>%
    mutate(source = "word")

  word_df <- rbind(word_df, text_df)
}

# select only required columns

word_df <- word_df %>%
  filter(text != "") %>%
  select(text, docname, doc_index, source)

# additional df - break down by sentences

word_df_sentences <- word_df %>%
  #unnest_tokens(words, text, tokens="unigram") %>%
  #unnest_tokens(sents, text, tokens="sentences") %>%
  unnest_sentences(sents, text) %>%
  rename (text = sents) %>%
  select(docname, text) %>%
  group_by(docname) %>%
  mutate(sentence_id = row_number()) %>%
  mutate(source = "word")

###############################################################################

# make initial df for pdf docs
pdf_df <- data.frame()

for (file in list_pdf_files){
  text_pdf <- paste0(folder_location, "/", file) %>%
    pdftools::pdf_text() %>%
    trimws() %>%
    str_replace_all("\\s+", " ") %>% # replace multiple whitespaces with a single whitespace
    unlist() %>%
    str_replace_all("\\s+", " ") %>% # replace multiple whitespaces with a single whitespace
    as_tibble() %>%
    rename(text = value) %>%
    mutate(docname = file) %>%
    mutate(doc_index = row_number()) %>%
    mutate(source = "pdf")

    text_pdf$text <- trimws(text_pdf$text)

  pdf_df <- rbind(pdf_df, text_pdf)
}

# select only required columns

pdf_df <- pdf_df %>%
  filter(text != "") %>%
  select(text, docname, doc_index, source)

# additional df - break down by sentences

pdf_df_sentences <- pdf_df %>%
  #unnest_tokens(words, text, tokens="unigram") %>%
  #unnest_tokens(sents, text, tokens="sentences") %>%
  unnest_sentences(sents, text) %>%
  rename (text = sents) %>%
  select(docname, text) %>%
  group_by(docname) %>%
  mutate(sentence_id = row_number()) %>%
  mutate(source = "word")

################################################################################

# join both doctypes together
df <- rbind(word_df, pdf_df)
df_sentences <- rbind(word_df_sentences, pdf_df_sentences)

# get unique documents
doc_names <- unique(df$docname)

# make df one large string for each doc
full_df <- data.frame()
for (i in doc_names){
  test <- df %>%
    filter(docname == i)

  text <- ""
  # for each row, combine the text
  for (ii in 1:nrow(test)){
    texter <- test$text[ii]
    text <- paste(text, texter)
  }
  full_df1 <- data.frame(text=as.character(text), docname=i, doc_index=1, source=test$source[1])
  full_df <- rbind(full_df, full_df1)
}
 # write csv
write.csv(df, row.names = FALSE, file = paste0(folder_location,"/","r_text.csv"))
write.csv(full_df, row.names = FALSE, file = paste0(folder_location,"/","r_full_text.csv"))
write.csv(df_sentences, row.names = FALSE, file = paste0(folder_location,"/","r_sentences.csv"))

###################################################################################################
# split long strings of text into chunks of text
n_char_lim <- 10

#initialise holding variables
#text_chunks <- list()
#document_name <- list()
chunk_df <- data.frame()
row_counter = 0
for (each_string in full_df$text){
  row_counter = row_counter + 1 # to capture docname source where exact same text entries exist
  #document <- full_df %>%
    #filter(text == each_string)
  
  
  #initialise holding variables
  text_chunks <- list()
  document_name <- list()
  current_chunk <- NULL#""
  document <- full_df$docname[[row_counter]]
  
  # split the text into full-stop sections
  split_strings <- strsplit(each_string, "(?<=[.])\\s+", perl=TRUE)[[1]]
  
  # check the length of these sections
  for (string in split_strings){
    
    # add/check for current_chunk length (keep adding text until limit reached)
    if(nchar(paste(current_chunk, string, sep = " ")) > n_char_lim){
      
      # split the string
      text_chunks <- c(text_chunks, current_chunk)
      current_chunk <- string
      
    } else {
      current_chunk <- paste(current_chunk, string, sep = " ")
    }
  }

  # add blocks of text together
  text_chunks <- c(text_chunks, current_chunk)
  document_name <- c(document_name, document)
  #print(document_name)
  
  # make df of these text_chunks
  chunk_df_test <- data.frame(text=unlist(text_chunks), docname=unlist(document_name))
  chunk_df <- rbind(chunk_df,chunk_df_test)
}

# write to csv text_chunks
write.csv(chunk_df, row.names = FALSE, file = paste0(folder_location,"/","r_text_chunks.csv"))

# make df of these text_chunks
#chunk_df <- data.frame(text=unlist(text_chunks), docname=unlist(document_name))