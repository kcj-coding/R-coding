library(stringr)

string <- "The Day123 was go0od.!?"

# extract lowercase only
lwr_wrds <- gsub("([^a-z])+", " ", string)

# make letters only
ltrs <- gsub("([^a-zA-Z])+", " ", string)

# make numbers only
nbrs <- gsub("([^0-9])+", " ", string)

# make punctuation only
punc <- str_extract_all(string, "([[:punct:]])+")

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
# or
ext <- str_extract_all(string1, "(?://{1,})([^*]+)")
#(?://{1,}) # find 1 or more instances of //
#([^*]+) # extract everything after this


# extract date only
date <- str_extract(string1, "\\d+/\\d+/\\d+")
# or
date_2 <- str_extract(string1, "(?://{1,})*(\\d+/\\d+/\\d+)")
#(?://{1,}) # find 1 or more instances of //
#(\d+/\d+/\d+) # extract numbers in the format d/d/d after this

# get numbers not joined to any words
std_nm <- str_extract(string1, "\\s\\d+\\s")
# or
std_nm2 <- str_extract(string1,"(?://{1,})*\\s(\\d+)\\s")
#(?://{1,}) # find 1 or more instances of //
#\s(\d+)\s # extract numbers in the format whitespace number whitespace

################################################################################

string2 = "The.Weather.is.nice.today./12abcdef"

# remove fullstops and ignore everything after the /
simple <- gsub("(.[^./]*$)", "", string2)
simple <- gsub('[.]', " ", simple)

# extract the number after the /
num_simple <- str_extract_all(string2, "([0-9]+)")

# extract 2 letter words
two_ltr <- str_extract_all(string2, "(\\b\\w{2,2}\\b)")
# (\b\w{2,2}\b) # find only whole words at word boundary \b and only show those of length 2 characters

################################################################################

string3 <- "thezzzzzzzzz day \\\\\ is be goood cc days"

# extract 2 letter words after the \\\\\
#ext1 = re.findall(r"(?:\\{1,})*\s(\w{2,2})(?=\s|$)",string3)
# (?:\\{1,})* #
# \s(\w{2,2}) #
# (?=\s|$) # 
ext1 <- str_extract_all(string3, "\\b\\w{2,2}\\b")

# only return whole words with the letter a in them
a <- str_extract_all(string3, "(?=[a-z]*a)[a-z]+")
# (?<![a-z]) # negative lookbehind # not needed
# (?=[a-z]*a) # positive lookahead to any words with a in them
# [a-z]+ # capture any words matching this condition

# extract only non duplicated letter words
non_dupe_ltr <- str_extract_all(string3, "\\b(?!\\w*?(\\w)\\1)(\\w+)")

# find instances of repeated letters in the string
rptd_ltrs <- str_extract_all(string3, "(\\w+)\\1")

################################################################################

string4 <- "The summary report was useful. The report had samples of data."

# find the first occurrence of report or sample, case insensitive
# which is separated by a word boundary, or begins with a number and then a word boundary

sngl_match <- str_extract(string4, "\\b([Rr]eport|[Ss]ample).*\\b.*|.*\\d+.*\\b([Rr]eport|[Ss]ample)\\b.*")

################################################################################

string5 <- "C:\\Users\\kelvi\\Documents"

# find any match to a letter "k" and return the entire string between \\
k_match <- str_extract_all(string5, "(?=k[\\w+]).*?(?=\\|\\/|(?=$))")

################################################################################

string6 <- "1. The day: The day was good. 2. The next day: The next day was 26.2C."

# find the numbers (with the fullstop) before the :
nbr_match <- str_extract_all(string6, "(\\d+)(?=\\D+\\:)")

################################################################################

string7 <- r"[C:\\Folder\File.xyz]" # read folder location and file

# get the file extension
file_ext <- str_extract_all(string7, "(\\.[^.]+)$")
file_ext_gsub <- gsub("(.+?)(?=\\.[^.]*$)", "", string7, perl=TRUE) # replace everything that is not found in the match (file extension)

################################################################################

# In R, if using strsplit or gsub you may need to use perl = TRUE e.g. txt_split <- strsplit(string6, "(\\d+)(?=\\D+\\:)", perl = TRUE) or gsub("([^0-9a-zA-Z])+", " ", string, perl=TRUE)