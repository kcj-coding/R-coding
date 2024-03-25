library(stringr)

string <- "The Day123 was go0od.!?"

# extract lowercase only
lwr_wrds <- gsub("([^a-z])+", " ", string)

# make letters only
ltrs <- gsub("([^a-zA-Z])+", " ", string)

# make numbers only
nbrs <- gsub("([^0-9])+", " ", string)

# make punctuation only
punc <- gsub("([[:punct:]])+", " ", string)

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