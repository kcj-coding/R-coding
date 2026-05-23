# https://p4a.seas.gwu.edu/2023-Spring/class/13-monte-carlo-methods/#17
# https://psyteachr.github.io/data-skills-v1/simulation.html

gc()
start_time <- Sys.time()

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

grapher_base <- function(dff, prob, xxx, xxx1, word, n) {
  plot(x=dff$event, y=dff$result, type="l", bty="l", lwd=1.5, xlab="Iterations", ylab="Probability", main=paste("Total probability of ", word, " outcome from coin flip", "\nn=",n,"; ",val1,"=",length(xxx),"; ",val2,"=",length(xxx1),sep=""))
  # loop to add lines for each in xxx and xxx1
  sapply(xxx, function(xint) abline(v = xint, col="#111212", lty=2))
  sapply(xxx1, function(xint) abline(v = xint, col="#42f5f2", lty=2))
  abline(h=prob, col="red", lty=2)

} 


coin_flip <- function(){
  x <- sample(x = c(val1,val2), size = 1, replace = TRUE, prob = c(prob1, prob2))
  if (x == val1){
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
      
    }
  }
  #results <- results+flip_result
  run_one <- dff[dff$run == 1,]
  y_lim <- range(dff$result)
  plot(x=run_one$event, y=run_one$result, ylim=y_lim, col=1, type="l", bty="l", lwd=1.5, xlab="Iterations", ylab="Probability", main=paste("Total probability of ",val1, " outcome from coin flip", "\nn=",n, "; runs=",runs, sep=""))
  # loop plot all lines for all others
  if (runs > 1){
    for (i in 2:max(runs)){
      run_x <- dff[dff$run == i,]
      lines(x=run_x$event,y=run_x$result,lwd=1.5,col=i)
    }
  }
  abline(h=prob1, col="red", lty=2)
  
  run_one <- dff[dff$run == 1,]
  y_lim <- range(1-dff$result)
  plot(x=run_one$event, y=1-run_one$result, ylim=y_lim, col=1, type="l", bty="l", lwd=1.5, xlab="Iterations", ylab="Probability", main=paste("Total probability of ",val2, " outcome from coin flip", "\nn=",n, "; runs=",runs, sep=""))
  # loop plot all lines for all others
  if (runs > 1){
    for (i in 2:max(runs)){
      run_x <- dff[dff$run == i,]
      lines(x=run_x$event,y=1-run_x$result,lwd=1.5, col=i)
    }
  }
  abline(h=prob2, col="red", lty=2)
  
  
  # plot last result
  if (i == n_runs){
    dff1 <- dff[dff$run==(n_runs),]
    heads <- dff1[(dff1$type==val1) & (dff1$run==(n_runs)),]
    tails <- dff1[(dff1$type==val2) & (dff1$run==(n_runs)),]
    
    grapher_base(dff1, prob1, heads$event, tails$event, val1, n)
    
    dff1$result <- 1-dff1$result
    grapher_base(dff1, prob2, heads$event, tails$event, val2, n)
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
      
      
    }
    
    dff1x <- dffx
    heads <- dff1x[(dff1x$type==val1),]
    tails <- dff1x[(dff1x$type==val2),]
    
    grapher_base(dff1x, prob1, heads$event, tails$event, val1, n)
    
    dff1x$result <- 1-dff1x$result
    grapher_base(dff1x, prob2, heads$event, tails$event, val2, n)
    
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

################################################################################

end_time <- Sys.time()
print(paste("runtime: ",end_time-start_time,sep=""))