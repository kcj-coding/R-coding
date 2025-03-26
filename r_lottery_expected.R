library(tidyr)
library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)

#r_df <- data.frame(a=rnorm(90), b=rnorm(90), c=rep(c("a","b"),times=45))

#r_df <- r_df |>
#  group_by(c) |>
#  mutate(cc=cumsum(a))|>
#  ungroup()

# https://socviz.co/refineplots.html
# https://intro2r.com/the-start-of-the-end.html

#r_dat <- data.frame(a=rnorm(90),b=rnorm(90))

#hist(r_dat$a, freq=FALSE)
#lines(x=density(x=r_dat$a), col="red")
#abline(v=mean(r_dat$a), col="red", lwd=2, lty='dashed')
#abline(v=median(r_dat$a), col="blue", lwd=2, lty='dashed')

# summary of df
#r_dat_summary <- summary(r_dat)

# view summary statistics
#library(summarytools)
#print(dfSummary(r_dat))

# stat graphs
#hist(r_dat$a)
#plot(r_dat$a)
#boxplot(r_dat$a)
#qqplot(r_dat$a,r_dat$a)


#t <- (factorial(49)/(factorial(6)*factorial(49-6))) # combination function

all_numbers_prob_simple <- function(m,k,n,b,bb,nb){
  
  # P(m of k from n) = math.choose(n,k)/(math.choose(k,m) * math.choose(n-k,k-m))
  
  P <- choose(n,k)/(choose(k,m) * choose(n-k,k-m))
  Pb <- choose(nb,bb)/(choose(bb,b) * choose(nb-bb,bb-b))
  
  val <- P*Pb
  
  return(val)
}

# match 5 of 5 balls with a range of 50 possible numbers, match 1 of 2 bonus balls with a range of 12 numbers
tst_euro <- all_numbers_prob_simple(5,5,50,1,2,12) # match 2 bonus balls

graph_expected_winners <- function(tickets_sold,probability_per_ticket,range){
  # Given parameters
  total_tickets <- tickets_sold  # Total number of tickets
  probability_of_winning <- probability_per_ticket  # Probability of winning on a single ticket
  
  # Calculate the expected number of winners (mean)
  expected_winners <- total_tickets * probability_of_winning
  cat("Expected number of winners:", expected_winners, "\n")
  
  # Range of number of winners to consider (we'll plot from 0 to a bit beyond the expected mean)
  x <- 0:(expected_winners+range)  # Number of winning tickets, can adjust upper limit based on expected winners
  
  # Binomial distribution probability mass function (PMF)
  binom_probs <- dbinom(x, size = total_tickets, prob = probability_of_winning)
  # Poisson
  poisson_probs <- dpois(x,expected_winners)
  
  #hist(binom_probs)
  
  # Plot the poisson distribution for the number of winners
  plot(x, poisson_probs, type = "h", col = "#33BBFF", lwd = 10,
       xlab = "Number of Winning Tickets", ylab = "Probability",
       main = paste("Poisson Approximation of lottery winners","\n","Total tickets =",total_tickets,"Probability per ticket = ",probability_of_winning))
  
  # Add a point for the expected number of winners
  abline(v = expected_winners, col = "red", lwd = 2, lty = 2)
  #text(expected_winners + 0.5, 0.02, paste("Expected winners: ", round(expected_winners, 2)), col = "red")
  
  # Add a legend
  legend("topright", legend = c("Poisson PMF", paste("Expected Number of Winners", round(expected_winners, 2))), 
         col = c("#33BBFF", "red"), lwd = c(2, 2), lty = c(1, 2))
  
  # Plot the binomial distribution for the number of winners
  plot(x, binom_probs, type = "h", col = "#33BBFF", lwd = 10,
       xlab = "Number of Winning Tickets", ylab = "Probability",
       main = paste("Binomial Approximation of lottery winners","\n","Total tickets =",total_tickets,"Probability per ticket = ",probability_of_winning))
  
  # Add a point for the expected number of winners
  abline(v = expected_winners, col = "red", lwd = 2, lty = 2)
  #text(expected_winners + 0.5, 0.02, paste("Expected winners: ", round(expected_winners, 2)), col = "red")
  
  # Add a legend
  legend("topright", legend = c("Binomial PMF", paste("Expected Number of Winners", round(expected_winners, 2))), 
         col = c("#33BBFF", "red"), lwd = c(2, 2), lty = c(1, 2))
}

# for 20 million tickets sold, how many winners can be expected for this winning number combination (probability of winning)
graph_expected_winners(20e6,1/tst_euro,15)

graph_expected_winners_ggplot <- function(tickets_sold,probability_per_ticket,range){
  # Given parameters
  total_tickets <- tickets_sold  # Total number of tickets
  probability_of_winning <- probability_per_ticket  # Probability of winning on a single ticket
  
  # Calculate the expected number of winners (mean)
  expected_winners <- total_tickets * probability_of_winning
  cat("Expected number of winners:", expected_winners, "\n")
  
  # Range of number of winners to consider (we'll plot from 0 to a bit beyond the expected mean)
  x <- 0:(expected_winners+range)  # Number of winning tickets, can adjust upper limit based on expected winners
  
  # Binomial distribution probability mass function (PMF)
  binom_probs <- dbinom(x, size = total_tickets, prob = probability_of_winning)
  # Poisson
  poisson_probs <- dpois(x,expected_winners)
  
  #hist(binom_probs)
  
  binom_df <- data.frame(x=x,y=binom_probs)
  poisson_df <- data.frame(x=x,y=poisson_probs)
  
  disp <- ggplot(binom_df, aes(x=x,y=y))+
    geom_bar(stat="identity", fill="#33BBFF")+
    geom_vline(aes(xintercept=expected_winners,color="red"), linetype="dashed")+
    #xlim(0,expected_winners+range)+
    #scale_linetype_manual(values=2)+
    scale_color_manual(name="Expected winners",labels=c(round(expected_winners,2)), values=c("red"))+
    #guides(color="none")+
    labs(x="Number of winning tickets", y="Probability", title=paste("Binomial Approximation of lottery winners","\n","Total tickets = ",total_tickets,", Probability per ticket = ",probability_of_winning,sep=""))+
    theme_classic()#+
    #theme(legend.position="none")
  
  disp1 <- ggplot(poisson_df, aes(x=x,y=y))+
    geom_bar(stat="identity", fill="#33BBFF")+
    geom_vline(aes(xintercept=expected_winners,color="red"), linetype="dashed")+
    #xlim(0,expected_winners+range)+
    #scale_linetype_manual(values=2)+
    scale_color_manual(name="Expected winners",labels=c(round(expected_winners,2)), values=c("red"))+
    #guides(color="none")+
    labs(x="Number of winning tickets", y="Probability", title=paste("Poisson Approximation of lottery winners","\n","Total tickets = ",total_tickets,", Probability per ticket = ",probability_of_winning,sep=""))+
    theme_classic()#+
  #theme(legend.position="none")
  
  print(disp)
  print(disp1)
  
}

graph_expected_winners_ggplot(20e6,1/tst_euro,15)