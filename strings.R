library(stringr)

string <- "The Day123 was go0od.!?"

# extract lowercase only
lwr_wrds <- gsub("([^a-z])+", " ", string)

# make letters only
ltrs <- gsub("([^a-zA-Z])+", " ", string)

# make numbers only
nbrs <- gsub("([^0-9])+", " ", string)

# make punctuation only
punc <- str_extract(string, "([[:punct:]])+")

# make numbers and letters only
nbrs_ltrs <- gsub("([^0-9a-zA-Z])+", " ", string)

# make lowercase
lwr <- tolower(string)

# make uppercase
upr <- toupper(string)

# split after fullstop
split_string <- str_split(string, "\\.")

# capitalise only the first word
cap <- str_to_sentence(string)

################################################################################

string1 <- "The.Day//Was1111111Good for the 10 of the month being 10/03/2024 a Sunday"

# extract everything after the //
ext <- gsub("\\W\\W+", " ", string1)

# extract date only
date <- str_extract(string1, "\\d+/\\d+/\\d+")

# get numbers not joined to any words
std_nm <- str_extract(string1, "\\s\\d+\\s")