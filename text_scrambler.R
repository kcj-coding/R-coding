library(dplyr)

gc()

letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
numbers <- "0123456789"

text <- "The day 123, 456, !@."

#t <- which(strsplit(numbers, "")[[1]]=="7")
#tt <- strsplit(numbers,"")
#ttt <- sapply(tt,"[[",t)

# get a random number, check if already captured and only continue when not existing entry
#num_chk <- TRUE
#x <- 0
#while (num_chk){
#  x <- x+1
#  # check num_chk
#  num_chk <- x <= 5
#}
#x <- x

# make functions
phrase_generation <- function(user_text){
 phrase <- NULL # NULL or ""
 for (word in 1:length(unlist((strsplit(user_text, "[\\s]", perl=TRUE))))){
   new_word <- NULL # NULL or ""
   word1 <- unlist((strsplit(text, "[\\s]", perl=TRUE)))[word]
   for (i in seq(1,nchar(word1),1)){
     letter <- substr(word1,i,i)
     if (grepl(letter, "[!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~]", perl=TRUE)){
       new <- letter
     } else if (grepl(letter, numbers, perl=TRUE)){
       new <- floor(runif(1,min=0,max=9))
     } else if (grepl(letter, "\n", perl=TRUE)){
       new <- letter
     } else {
       new_letter_num <- floor(runif(1,min=1, max=length(letters)))
       new <- letters[new_letter_num]
     }
     if (grepl("([[:upper:]])", letter, perl=TRUE)){#substr(word1,i,i))){
       new <- toupper(new)
     }
     phrase <- append(phrase, paste(new, "", collapse='', sep=""))
     #phrase <- paste(as.character(phrase),collapse='',sep="")
   }
   phrase <- append(phrase, paste("", " ", collapse='', sep=""))
   phrase <- paste(as.character(phrase),collapse='',sep="")
 }
 phrase <- substr(phrase,0,nchar(phrase)-1)
 return (phrase)
}

###############################################################################

phrase_generation_same_mappings <- function(user_text){
  xyz <- NULL # NULL or ""
  xtras <- NULL # NULL or ""
  phrase <- NULL # NULL or ""
  for (word in 1:length(unlist((strsplit(user_text, "[\\s]", perl=TRUE))))){
    new_word <- NULL # NULL or ""
    word1 <- unlist((strsplit(text, "[\\s]", perl=TRUE)))[word]
    for (i in seq(1,nchar(word1),1)){
      letter <- substr(word1,i,i)
      letteri <- tolower(letter)
      if (letteri == "."){
        new <- letteri
        xtras <- append(xtras,new)
      } else {
      #print(paste("is . =",letteri,grepl(letteri,".")))
      if (grepl(letteri, paste(xyz,collapse='', sep=""))){
        t <- which(xyz==letteri) # capture first instance or %in%
        #t <- unlist(regexp(letteri,xyz)) # alternative method
        # use existing match
        #xtras <- append(xtras, paste(xtras[t], "", collapse='', sep=""))
        #xyz <- append(xyz, paste(letteri, "", collapse='', sep=""))
        #tt <- strsplit(paste(xtras,collapse='', sep=""),"")

        new <- xtras[t]#sapply(xtras, "[[",t-1)
    } else {
      xyz <- append(xyz, letteri)
      
      if (letteri == "."){
        new <- letteri
        xtras <- append(xtras,new)
      } else if (grepl(letter, "[!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~]", perl=TRUE)){
        new <- letter
        xtras <- append(xtras, new)
      } else if (grepl(letter, numbers, perl=TRUE)){
        new <- floor(runif(1,min=0,max=9))
        xtras <- append(xtras, new)
      } else if (grepl(letter, "\n", perl=TRUE)){
        new <- letter
        xtras <- append(xtras, new)
      } else{
        # loop until find an unused letter (eventually all 26 letters will be used)
        # get a random number, check if already captured and only continue when not existing entry
        if (length(xtras)>0){
        num_chk <- TRUE
        while (num_chk){
          #print("i")
          new_letter_num <- floor(runif(1,min=1, max=length(letters)))
          new <- letters[new_letter_num] # where no existing letter used
          #print(grepl(new,xtras, perl=TRUE))
          # check num_chk
          
          if (any(new==xtras)){
            num_chk <- TRUE
          } else {
            num_chk <- FALSE
          }
        }} else {
          new_letter_num <- floor(runif(1,min=1, max=length(letters)))
          new <- letters[new_letter_num] # where no existing letter used
        }
        xtras <- append(xtras, new)
      }}}
      if (grepl("([[:upper:]])", letter, perl=TRUE)){#substr(word1,i,i))){
        new <- toupper(new)
      }
      phrase <- append(phrase, paste(new, "", collapse='', sep=""))
      #phrase <- paste(as.character(phrase),collapse='',sep="")
    }
    phrase <- append(phrase, paste("", " ", collapse='', sep=""))
    phrase <- paste(as.character(phrase),collapse='',sep="")
  }
  phrase <- substr(phrase,0,nchar(phrase)-1)
  return (phrase)
}
###############################################################################


print(paste("original phrase: ", text, sep=""))
print(paste("random phrase: ", phrase_generation(text), sep=""))
print(paste("scrambled phrase: ", phrase_generation_same_mappings(text), sep=""))