#library(tidyverse)
#library(dplyr)
library(ggplot2)
#library(summarytools)

# https://p4a.seas.gwu.edu/2023-Spring/class/13-monte-carlo-methods/#17
# https://psyteachr.github.io/data-skills-v1/simulation.html

############### configure variables ############################################

val1 <- "heads"
prob1 <- 0.7

val2 <- "tails"
prob2 <- 0.3

n_runs <- 5
n_events <- 50

################################################################################

# check probability sum to 1
if (prob1+prob2 != 1){
  stop("Probability values do not sum to 1")
}

grapher <- function(dff, prob, xxx, xxx1, word, n){
  tt <- ggplot(dff,aes(x=event,y=result))+
    sapply(xxx, function(xint) geom_vline(aes(xintercept = xint), color="#111212", linetype="dotted"))+
    sapply(xxx1, function(xint) geom_vline(aes(xintercept = xint), color="#42f5f2", linetype="dotted"))+
    geom_line()+
    geom_hline(yintercept=prob, linetype="dotted", color="red")+
    labs(x="Iterations",y="Probability",title=paste("Total probability of ", word, " outcome from coin flip", "\nn=",n,"; ",val1,"=",length(xxx),"; ",val2,"=",length(xxx1),sep=""))+
    theme_classic()
  print(tt)
}

coin_flip <- function(){
  x <- sample(x = c(val1,val2), size = 1, replace = TRUE, prob = c(prob1, prob2))
  if (x == "heads"){
    val <- 1
   } else{
      val <- 0
  }
  return(list(type=x,result=val))
}

t <- coin_flip()$type

dff <- data.frame()

monte_carlo_coin_xtra <- function(runs, n, dff){
  
  for (i in 1:runs){
    results <- 0
    for (ii in 1:n){
      flip_result <- coin_flip()$result
      typer <- coin_flip()$type
      results <- results+flip_result
      prob_value <- results/(ii+1)
      
      df1 <- data.frame(run=i,event=ii,result=prob_value,type=typer)
      dff <- rbind(dff,df1)
      
      #t <- ggplot(dff, aes(x=event, y=result, group=run))+
      #  geom_line(color=run)
      
    }
  }
  #results <- results+flip_result
  t <- ggplot(dff, aes(x=event, y=result, group=run, color=run))+
    geom_line(color=dff$run)+
    labs(x="Iterations", y="Probability", title=paste("Total probability of ",val1, " outcome from coin flip", "\nn=",n, "; runs=",runs, sep=""))+
    theme_classic()
    
    tt <- ggplot(dff, aes(x=event, y=1-result, group=run, color=run))+
      geom_line(color=dff$run)+
      geom_hline(yintercept=prob2, linetype="dotted", color="red")+
      labs(x="Iterations", y="Probability", title=paste("Total probability of ",val2, " outcome from coin flip", "\nn=",n, "; runs=",runs, sep=""))+
      theme_classic()
    
  print(t)
  print(tt)
  
  # plot last result
  if (i == n_runs){
    dff1 <- dff[dff$run==(n_runs),]
    heads <- dff1[(dff1$type==val1) & (dff1$run==(n_runs)),]
    tails <- dff1[(dff1$type==val2) & (dff1$run==(n_runs)),]
    
    grapher(dff1, prob1, heads$event, tails$event, val1, n)
    
    dff1$result <- 1-dff1$result
    grapher(dff1, prob2, heads$event, tails$event, val2, n)
  }
  
  return (dff)# sum(dff$result)/(runs*n)
}

monte_carlo_coin <- function(n){
  dffx <- data.frame()
    results <- 0
    for (ii in 1:n){
      flip_result <- coin_flip()$result
      typer <- coin_flip()$type
      results <- results+flip_result
      prob_value <- results/(ii+1)
      
      df1 <- data.frame(event=ii,result=prob_value,type=typer)
      dffx <- rbind(dffx,df1)
      
      #t <- ggplot(dff, aes(x=event, y=result, group=run))+
      #  geom_line(color=run)
      
    }
    
    dff1x <- dffx
    heads <- dff1x[(dff1x$type==val1),]
    tails <- dff1x[(dff1x$type==val2),]
    
    grapher(dff1x, prob1, heads$event, tails$event, val1, n)
    
    dff1x$result <- 1-dff1x$result
    grapher(dff1x, prob2, heads$event, tails$event, val2, n)
    
    return (dffx)# sum(dff$result)/(n)
  }
  

################################################################################

tst <- monte_carlo_coin_xtra(n_runs,n_events,dff)

tst1 <- monte_carlo_coin(50)


coin <- c("heads", "tails")
N <- 50000
tosses <- sample(x = coin, size = N, replace = TRUE, prob = c(0.5, 0.5))

heads <- sum(tosses == "heads")/N
tails <- sum(tosses == "tails")/N
