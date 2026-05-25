gc()
start_time <- Sys.time()

################################################################################

iterations <- 500 # number of simulations to run
days <- 10 # number of days to forecast into the future
threshold <- 5 # number of events to not exceed
tag <- "Events"

historical <- c(4,3,5,6,2,4,5,7,8,3,2,4,5,6) # historical events e.g per day
# or csv read
#historical <- read.csv()

# or just a mean value
#historical <- 4

################################################################################

mean_val <- mean(historical) # mean numbers of tickets
sd_val <- ifelse(length(historical)>1,sd(historical),1) # standard deviation in ticket numbers

#################### functions #################################################

distribution <- function(runs,mean_val,sd_val,type){
  if (type == "Normal"){
    sim <- rnorm(iterations, mean=mean_val, sd=sd_val)
  } else if (type == "Poisson"){
    sim <- rpois(n=iterations, lambda=mean_val)
  }
  sim <- round(sim,0)
  
  sim <- ifelse(sim<0,0,sim)#sim[sim<0] <-0
  
  exp <- mean(sim)
  lower <- quantile(sim, 0.05)
  upper <- quantile(sim, 0.95)
  iqr <- IQR(sim) # also upper-lower
  
  cat("Expected for",type, "distribution", round(exp,1), "\n")
  cat("90% confidence interval for",type, round(lower,1), "-", round(upper,1),"\n")
  
  prob_exceed <- mean(sim > threshold)
  cat("Probability of exceeding threshold for",type, threshold, "is", round(prob_exceed*100,2),"%\n")
  
  hist(sim,freq=FALSE,
       #breaks=5,
       col="#34b7eb",border="black",
       main=paste("Daily number of ",tag," for " ,type," distribution","\n Runs=",iterations," Mean:",round(exp,2)," sd=",round(sd_val,2)," IQR=",round(iqr,2),sep=""),
       xlab=paste("Number of ",tag,sep=""),bty="l")
  lines(density(sim), lwd=2)
  abline(v=exp, col="red", lwd=2, lty=2)
}

###### distributions ###########################################################

distribution(iterations,mean_val,sd_val,"Normal")
distribution(iterations,mean_val,sd_val,"Poisson")

################################################################################

# forecasting

forecast <- function(runs,data,data1,days,threshold,type){
  for (i in 1:runs) {
    lambda_day <- sample(data1, days, replace = TRUE)
    decay <- runif(days, min = 0.01, max = 0.05) # percentage that lambda value can change by day, min to max bounds
    lambda_adj <- lambda_day * (1 - decay)
    data[, i] <- rpois(days, lambda_adj) # replace values with poisson distribution of new mean lambda_adj
  }
  
  if(type=="Cumulative"){
    data <- apply(data, 2, cumsum)
    threshold <- threshold*days
  }
  
  # Daily prediction intervals
  pi50 <- apply(data, 1, quantile, probs = c(0.25, 0.75))
  pi80 <- apply(data, 1, quantile, probs = c(0.10, 0.90))
  pi95 <- apply(data, 1, quantile, probs = c(0.025, 0.975))
  pi99 <- apply(data, 1, quantile, probs = c(0.005, 0.995))
  pi999 <- apply(data, 1, quantile, probs = c(0.0005, 0.9995))
  
  mean_line <- apply(data, 1, mean)
  
  # graph
  # Base plot (blank)
  plot(1:days, mean_line, type="n",ylim=range(0,max(data)),
       xlab="Day", ylab=paste(type," ",tag,sep=""),
       main=paste(type," ",tag," with shaded uncertainty bands","\n","Runs=",runs," Threshold=",threshold,sep=""),bty="l")
  
  # Add simulation lines
  matlines(1:days, data, col="#34b7eb", lty=1)
  
  # Add mean line
  lines(1:days, mean_line, lwd=1.5, col="black")
  
  abline(h=threshold, col="red", lwd=2, lty=2)
  
  # graph with shaded regions
  # Base plot (blank)
  plot(1:days, mean_line, type="n",,ylim=range(0,max(data)),
       xlab="Day", ylab=paste(type," ",tag,sep=""),
       main=paste(type," ",tag," with shaded uncertainty bands","\n","Runs=",runs," Threshold=",threshold,sep=""),bty="l")
  
  # 99.9% band (widest)
  polygon(
    c(1:days, rev(1:days)),
    c(pi999[1,], rev(pi999[2,])),
    col="#42f5e3", border=NA
  )
  
  # 99% band (widest)
  polygon(
    c(1:days, rev(1:days)),
    c(pi99[1,], rev(pi99[2,])),
    col="#42e3f5", border=NA
  )
  
  # 95% band (second widest)
  polygon(
    c(1:days, rev(1:days)),
    c(pi95[1,], rev(pi95[2,])),
    col="#34b7eb", border=NA
  )
  
  # 80% band
  polygon(
    c(1:days, rev(1:days)),
    c(pi80[1,], rev(pi80[2,])),
    col="#3483eb", border=NA
  )
  
  # 50% band (tightest)
  polygon(
    c(1:days, rev(1:days)),
    c(pi50[1,], rev(pi50[2,])),
    col="#343aeb", border=NA
  )
  
  # Add simulation lines
  matlines(1:days, data, col=rgb(0,0,1,0.05), lty=1)
  
  # Add mean line
  lines(1:days, mean_line, lwd=1.5, col="black")
  
  abline(h=threshold, col="red", lwd=2, lty=2)
  
  leg_cols <- c("#42f5e3","#42e3f5","#34b7eb","#3483eb","#343aeb")
  leg_sym <- c(16, 16, 16,16,16)
  leg_lab <- c("99.9%", "99%", "95%","80%","50%")
  
  legend(x = 1, y = max(data), col = leg_cols, pch = leg_sym, 
         legend = leg_lab, bty = "n", 
         title = "Legend")
  
  return(data)
}

############ forecastings ######################################################

sim_matrix <- matrix(NA, nrow = days, ncol = iterations)

matrix1 <- forecast(iterations,sim_matrix,historical,days,threshold,"Daily")

matrix2 <- forecast(iterations,sim_matrix,historical,days,threshold,"Cumulative")


################################################################################

# probability of exceeding threshold

exceed_threshold <- function(data,days,threshold) {
  # For each day, compute probability of exceeding threshold
  prob_exceed_daily <- colMeans(t(data) > threshold) # this is less than the number of runs e.g. how many times on average has threshold been exceeded
  prob_exceed_daily
  
  plot(1:days, prob_exceed_daily, type="b", pch=19,,ylim=range(0,1),
       xlab="Day", ylab="Probability",
       main=paste("Probability of exceeding daily threshold","\n","Threshold=",threshold," Risk is <=",round(mean(prob_exceed_daily),2),sep=""),bty="l")
  
  abline(h = mean(prob_exceed_daily), col="red", lwd=2, lty=2)
  
  cum_matrix <- apply(data, 2, cumsum)
  
  cum_threshold <- threshold*days
  
  prob_exceed_cumulative <- mean(cum_matrix[days, ] > cum_threshold)
  prob_exceed_cumulative
  
  prob_exceed_any_day <- mean(apply(data, 2, function(x) any(x > threshold)))
  prob_exceed_any_day
  
  days_exceeded <- colSums(data > threshold)
  
  table(days_exceeded) / iterations
  
  thresholds <- 1:max(cum_threshold)
  
  risk_curve <- sapply(thresholds, function(th) mean(data > th))
  
  pi95 <- apply(data, 1, quantile, probs = c(0.025, 0.975))
  
  plot(thresholds, risk_curve, type="b", pch=19,ylim=range(0,1),
       xlab=paste("Daily ",tag," Threshold",sep=""),
       ylab="Probability of Exceedance",
       main=paste("Threshold–Risk Curve","\n","Threshold=",threshold," Risk is <=",round(risk_curve[threshold],2),sep=""),
       bty="l")
  
  abline(v = threshold, col="red", lwd=2, lty=2)
  abline(h = risk_curve[threshold], col="red", lwd=2, lty=2)
  
  polygon(
    x = c(1:days, rev(1:days)),
    y = c(rep(threshold, days), rev(pi95[2,])),
    col = rgb(1,0,0,0.1), border = NA
  )
}

#### calculate #################################################################

exceed_threshold(matrix1,days,threshold)

################################################################################

end_time <- Sys.time()
print(paste("runtime: ",end_time-start_time,sep=""))