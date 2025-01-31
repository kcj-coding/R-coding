library(dplyr)

text <- "The day was nice. 7,8,94,7 7 7 8 8"

letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
numbers <- "0123456789"

use_same_mappings <- TRUE

# for each word in text, replace each letter by a random letter from letters
phrase <- NULL

mappingsi <- data.frame()
for (word in 1:length(unlist((strsplit(text, " "))))){
  word1 <- unlist((strsplit(text, " ")))[word]
  new_word <- NULL
  mappings <- list()
  for (i in seq(1,nchar(word1),1)){
    if (use_same_mappings == TRUE & nrow(mappingsi)>0 & substr(word1,i,i) %in% mappingsi$old){
        new <- mappingsi[mappingsi$old == substr(word1,i,i),]
        new <- new$new[1]
    }
    else if (grepl("([[:punct:]])+", substr(word1,i,i))){
      new <- substr(word1,i,i)
    }
    else if (grepl("([[:digit:]])+", substr(word1,i,i))){
      new <- floor(runif(1,min=0,max=9))
    }
    else {
      new_letter_num <- floor(runif(1,min=1, max=length(letters)))
      new <- letters[new_letter_num]
      if (grepl("([[:upper:]])", substr(word1,i,i))){
        new <- toupper(new)
      }
    }
    mappings[[i]] <- data.frame(old=as.character(substr(word1,i,i)),new=as.character(new))
    
    new_word <- paste(as.character(append(new_word, paste(new, collapse = '', sep=""))),collapse='',sep="")
    new_word1 <- as.character(new)
  }
  mappings <- do.call(rbind,mappings)
  mappingsi <- rbind(mappingsi,mappings)
  mappingsi <- mappingsi[!duplicated(mappingsi),]
  phrase <- append(phrase, paste(new_word, " ", collapse='', sep=""))
  phrase <- paste(as.character(phrase),collapse='',sep="")
}

print(paste("original phrase: ", text, sep=""))
print(paste("scrambled phrase: ", phrase, sep=""))