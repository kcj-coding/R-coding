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
