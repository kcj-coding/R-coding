
text <- "The day was good. uhdfgfhjfghfv hbdfuygfgfjf 12345 hgf"
words <- unlist((strsplit(text, "[\\s]", perl=TRUE)))

lim <- 5
sym <- ">>> " # space after it
letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
thing <- c("\\,","\\.") # as symbol not wildcard
print(paste(thing, collapse = "|"))
for (let in letters){
  print(let)
  print(grepl(paste(thing, collapse = "|"),let,perl=TRUE))
}
########################
#for (word in 1:length(unlist((strsplit(user_text, "[\\s]", perl=TRUE))))){

phrase_split <- function(user_text,lim){
  phrase <- NULL # NULL or ""
  total_i <- 0
  counter <- 0
  for (word in 1:length(unlist((strsplit(user_text, "[\\s]", perl=TRUE))))){
    new_word <- NULL # NULL or ""
    word1 <- unlist((strsplit(text, "[\\s]", perl=TRUE)))[word]
    word2 <- unlist((strsplit(text, "[\\s]", perl=TRUE)))[word+1]
    word2 <- ifelse(is.na(word2),"",word2)
    #print(word2)
    total_i <- nchar(word1)+total_i
    #print(total_i)
    #counter <- 0
    for (i in seq(1,nchar(word1),1)){ # if i is lim or total_i is a modulo of lim (total_i %% lim == 0)
      counter <- counter + 1
      letter <- substr(word1,i,i)
      letter2 <- substr(word1,i+1,i+1)
      letter2 <- ifelse(is.na(letter2),"",letter2)
      if (counter == 1){
        new <- paste("\n",sym,letter,sep="")
      # if number is a modulo of limit and is not a fullstop or comma and word2 is not another word
      #if (any(i == lim | i %% lim == 0 | counter %% lim == 0) & !any(grepl(letters,letter2,perl=TRUE)) & !all(grepl(thing,word2,perl=TRUE)) & !all(grepl(letters,word2,perl=TRUE))){
      } else if (all(counter %% lim == 0 && !all(grepl(paste(letters, collapse = "|"),letter2,perl=TRUE)) && !all(grepl(paste(thing, collapse = "|"),word2,perl=TRUE)) && !all(grepl(paste(letters, collapse = "|"),word2,perl=TRUE)))){
        new <- paste("\n",sym,letter,sep="")
        
      #} else if(any(i == lim | i %% lim == 0 | counter %% lim == 0) & any(grepl(letters,word2,perl=TRUE)) & !any(grepl(letters,letter2,perl=TRUE))) {
      } else if(all(counter %% lim == 0 && !any(grepl(paste(letters, collapse = "|"),letter2,perl=TRUE)))) {
        new <- paste("\n",sym,letter,sep="")
      #} else if(any(i == lim | i %% lim == 0 | counter %% lim == 0) & any(grepl(letters,letter2,perl=TRUE))) {
      } else if(all(counter %% lim == 0 && grepl(paste(letters, collapse = "|"),letter,perl=TRUE) && grepl(paste(letters, collapse = "|"),letter2,perl=TRUE)) && !grepl(paste(thing, collapse = "|"),letter,perl=TRUE)) {
        new <- paste("-\n",sym,letter,sep="")
        
      } else if(all(counter %% lim == 0 && all(letter2=="" | grepl(paste(thing, collapse = "|"),letter,perl=TRUE)))) {
        new <- paste("\n",sym,letter,sep="")
        
      } else if(all(counter %% lim == 0 && grepl(paste(letters, collapse = "|"),letter,perl=TRUE) && grepl(paste(letters, collapse = "|"),letter2,perl=TRUE)) && grepl(paste(thing, collapse = "|"),letter,perl=TRUE)) {
        new <- paste("\n",sym,letter,sep="")
        
      } else if(all(counter %% lim == 0 && grepl(paste(thing, collapse = "|"),letter,perl=TRUE))) {
          new <- paste("\n",sym,letter,sep="") 
      #} else if(any(counter %% lim == 0) & any(grepl(paste(letters, collapse = "|"),letter,perl=TRUE)) & any(grepl(paste(letters, collapse = "|"),letter2,perl=TRUE))) {
      #  new <- paste("-\n",sym,letter,sep="")
      #} else if(any(i == lim | i %% lim ==0 | counter %% lim ==0)) {
      #  new <- paste("-\n",sym,letter,sep="")
      } else { # if word2 is a word then... # if if number is a modulo of limit and the next entry is a letter add a hyphen -
        new <- letter
      }
      phrase <- append(phrase, paste(new, "", collapse='', sep=""))
    }
    phrase <- append(phrase, paste("", " ", collapse='', sep=""))
    phrase <- paste(as.character(phrase),collapse='',sep="")
  }
  phrase <- substr(phrase,1,nchar(phrase)-1)
  return (phrase)
}



cat(phrase_split(text,lim))