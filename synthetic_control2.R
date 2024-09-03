# https://bookdown.org/mike/data_analysis/synthetic-control.html
# https://carlos-mendez.quarto.pub/r-synthetic-control-tutorial/
library("Synth")
#library(SCtools)
#library("gsynth")

#options(repr.plot.width=20, repr.plot.height=10)

# make up some data
countries <- rep(c("England","France","USA"),each=60)
some_data <- rnorm(60*3,mean=2,sd=1)
more_data <- rnorm(60*3,mean=2,sd=1)
some_y_data <- rnorm(60*3,mean=2,sd=1)*10
time <- seq(as.Date("2015-01-01"), by="month", length.out=60) # can also just be numbers e.g. seq(1,90,1)
time <- rep(time,3)

df <- data.frame(countries=countries, time=time, data=some_data, more_data=more_data, y=some_y_data)


# make time as UK date
df$time_date <- format(df$time,"%d/%m/%y")

# get month and year
df$mnth_yr <- format(df$time,"%b %Y")

# date as integers
df$int_date <- rep(seq(1,60,1),3)

################################################################################

# add to df

# create variable number for countries
countries_num <- as.integer(rep(c(1,2,3),each=60)) # must be int

# add this to df
df$countries_num <- countries_num

# make a treatment indicator - needs to be balanced, e.g. not heavily in favour of start or end
df$treatment <- ifelse(df$countries == "France" & df$time >= as.Date("2016-03-01"), 1, 0)

# find time difference - credit to https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

time_dff <- elapsed_months(as.Date("2016-03-01"),as.Date("2015-01-01"))

# create some change/variable to the treated, to measure impact of treatment
df$y <- ifelse(df$countries == "France" & df$time >= as.Date("2016-03-01"), 
               df$y + 20, df$y)

# remove not needed columns
df <- select(df,-time,-mnth_yr,-time_date)

# arrange data so that it is y, data, countries,time,countries_num,treatment
df <- select(df, y, data, more_data, countries, int_date, countries_num, treatment)

################################################################################

# plot the data to see general trends
df_treated <- df %>%
  filter(countries=="France")

ggplot(df_treated,aes(x=int_date,y=y))+
  geom_line()+
  labs(x="Date",y="Number",title="")+
  theme_classic()

################################################################################

set.seed(1)

dataprep.out <-
  dataprep(
    as.data.frame(df),
    predictors            = c("data","more_data"),
    dependent             = "y",
    unit.variable         = "countries_num",
    time.variable         = "int_date",
    unit.names.variable   = "countries",
    treatment.identifier  = 2, # value and unit identifier
    controls.identifier   = c(1:1,3:3), # which state.num
    time.predictors.prior = c(1:14), # how many instances before change
    time.optimize.ssr     = c(1:14),
    time.plot             = c(1:60) # range of dataset for each unit
  )

synth.out <- synth(dataprep.out)

print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

################################################################################

path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Y"),
          Xlab         = c("Date"),
          Main         = c("Graph of treated and synthetic control of treated"),
          Legend       = c("France","Synthetic France"),
          Legend.position = c("topleft")
)

abline(v   = 15,
       lty = 2)

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap"),
          Xlab         = c("Date"),
          Ylim         = c(-30, 30),
          Main         = "Residuals between treatment and synthetic control"
)

abline(v   = 15,
       lty = 2)

################################################################################

# single placebo test

dataprep.out <-
  dataprep(
    as.data.frame(df),
    predictors            = c("data","more_data"),
    dependent             = "y",
    unit.variable         = "countries_num",
    time.variable         = "int_date",
    unit.names.variable   = "countries",
    treatment.identifier  = 1, # value and unit identifier of untreated
    controls.identifier   = c(2:3), # which state.num
    time.predictors.prior = c(1:14), # how many instances before change
    time.optimize.ssr     = c(1:14),
    time.plot             = c(1:60) # range of dataset for each unit
  )

synth.out <- synth(dataprep.out)

print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

################################################################################

path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Y"),
          Xlab         = c("Date"),
          Main         = c("Graph of treated and synthetic control of treated"),
          Legend       = c("England","Synthetic England"),
          Legend.position = c("topleft")
)

abline(v   = 15,
       lty = 2)

gaps.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Gap"),
          Xlab         = c("Date"),
          Ylim         = c(-30, 30),
          Main         = "Residuals between treatment and synthetic control"
)

abline(v   = 15,
       lty = 2)

################################################################################

# make some false/placebo data, and compare/evaluate to treated

store <- matrix(NA,length(1:60),length(unique(df$countries)))
colnames(store) <- unique(df$countries)#[-1]

# run placebo test
for(iter in 2:length(unique(df$countries))) # for number of other countries e.g. 2 others
{

  dataprep.out <-
    dataprep(
      foo=as.data.frame(df),
      predictors            = c("data","more_data"),
      time.predictors.prior = c(10:14),
      dependent             = "y",
      unit.variable         = "countries_num",
      time.variable         = "int_date",
      unit.names.variable   = "countries",
      treatment.identifier = iter,
      controls.identifier = c(1:3)[-iter],
      time.optimize.ssr = c(1:14),
      time.plot = c(1:60)
    )
  
  # run synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method = "BFGS"
  )
  
  # store gaps
  store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# then remove unreasonable predictions/data

# now do figure
data <- store
rownames(data) <- 1:60

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- 60#nrow(data)
years         <- 1:60
gap.end.pre  <- which(rownames(data)==14)

#  MSPE Pre-Treatment
mse        <-             apply(data[ gap.start:gap.end.pre,]^2,2,mean)
basque.mse <- as.numeric(mse[2]) # the treated mse
# Exclude states with 5 times higher MSPE than basque
data <- data[,mse<5*basque.mse]
Cex.set <- .75

# add on real France results
real_res <- df %>%
  filter(countries=="France")

rownames(real_res) <- 1:60

real_res <- select(real_res, y)
colnames(real_res) <- "France_real"

# add on to the data
data <- cbind(data,real_res)

# then graph this data

# Plot
plot(years,data[gap.start:gap.end,which(colnames(data)=="France_real")],
     ylim=c(-30,30),xlab="year",
     xlim=c(1,60),ylab="Gap in real GDPpc",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add France Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="France_real")],lwd=2,col="black")

# Add grid
abline(v=14,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomright",legend=c("France","control countries"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
abline(v=1)
abline(v=60)
abline(h=-2)
abline(h=2)



################################################################################
#gsynth.out <- gsynth(
#  y ~ treatment + data+more_data,
#  data = df,
#  index = c("countries", "int_date"), # group and time indicators
#  force = "two-way", # none, unit, time or two-way
#  CV = TRUE,
#  r = c(0, length(df)-2),
#  se = TRUE,
#  inference = "parametric",
#  nboots = 1000,
#  parallel = F # TRUE
#)

#plot(gsynth.out)

#plot(gsynth.out, type = "counterfactual", raw = "all")