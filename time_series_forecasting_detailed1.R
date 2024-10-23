library(dplyr)
library(fpp3)
# install.packages("urca")
# https://otexts.com/fpp3/
library(ggplot2)

output_folder <- "C:\\Users\\kelvi\\Desktop\\"

graph_title = "Number by day UK"

rows_to_use <- 5 # use this number of rows from the end of the df for model testing, the rest for model training

# get data in timeseries format - month year, date etc

# create some dates

some_dates <- data.frame(dates=seq(as.Date("2014-03-09"), by = "day", length.out = 30)) # yyyy-mm-dd format

# note that for fpp3 package dates have to be in a certain format https://otexts.com/fpp3/tsibbles.html

# and the index has to be the same object

########
# also note that for ranges e.g. months, dates, they should be sequential and if not existing for a day/month replace with 0 for true time picture

# e.g. create range of dates and then left_join data on these dates, and fill blank/NA with 0
#######

# convert to dates
dates <- data.frame(dates=format(as.Date(some_dates$dates),format="%b-%Y"))

# create some data
categories <- rep(c("England","France"),each=30)
some_data <- rnorm(60,mean=2,sd=1)*45

# make into df - time as first column
df <- data.frame(Day=seq(as.Date("2014-03-09"), by = "day", length.out = 30), countries=categories, Data=some_data)

# put into tsibble format

df$Day <- as_date(df$Day)
df$Day <- ymd(df$Day)
#df$Day <- as.POSIXct.Date(df$Day)

# filter for graph and make tsibble
df_eng <- df %>%
  filter(countries == "England") %>%
  select(-countries) %>%
  select(Day, Data) %>%
  as_tsibble(index=Day)

# check a tsibble and has a ts element [1D] or M or Y
print(df_eng)

# view a plot of the timeseries data
ggplot(df_eng, aes(x=Day, y=Data))+
  geom_line()+
  labs(x="Day", y="Number", title=graph_title) +
  theme_classic()
ggsave(paste(output_folder,"basic_graph1.png",sep=""),width=30,height=15,units="cm")

#dev.print(png,paste(output_folder,"chart.png",sep=""),width=1920,height=1080)
autoplot(df_eng,Data) +
  labs(title = graph_title,
       y="Number") +
  theme_classic()
ggsave(paste(output_folder,"basic_graph2.png",sep=""),width=30,height=15,units="cm")

# check for differences, make sure stationary

# print(df_eng |> features(Data, unitroot_kpss)) # if pvalue is 0.01, it can be <= 0.01. If 0.1 it can be >= 0.1

df_eng %>%
  gg_tsdisplay(difference(Data, 1),
               plot_type='partial', lag=5) +
  labs(title="Seasonally differenced", y="")
ggsave(paste(output_folder,"differences1.png",sep=""),width=30,height=15,units="cm")

# get ACF and PACF plots
df_eng |> ACF(difference(Data)) |> autoplot() +
  theme_classic()
ggsave(paste(output_folder,"differences1_acf.png",sep=""),width=30,height=15,units="cm")

df_eng |> PACF(difference(Data)) |> autoplot() +
  theme_classic()
ggsave(paste(output_folder,"differences1_pacf.png",sep=""),width=30,height=15,units="cm")

# difference again
df_eng %>%
  gg_tsdisplay(difference(Data, 1) |> difference(),
               plot_type='partial', lag=5) +
  labs(title = "Double differenced", y="")
ggsave(paste(output_folder,"differences2.png",sep=""),width=30,height=15,units="cm")

# get ACF and PACF plots
df_eng |> ACF(difference(Data) |> difference()) |> autoplot() +
  theme_classic()
ggsave(paste(output_folder,"differences2_acf.png",sep=""),width=30,height=15,units="cm")

df_eng |> PACF(difference(Data) |> difference()) |> autoplot() +
  theme_classic()
ggsave(paste(output_folder,"differences2_pacf.png",sep=""),width=30,height=15,units="cm")

################################################################################

########### train test split the data ##########################################
train <- df_eng[1:(nrow(df_eng)-rows_to_use),]
test <- df_eng[(nrow(df_eng)-rows_to_use):nrow(df_eng),]

# fit some models - you can either loop through parameters or manually specify some or use automated fitting

# model the data with different model types and pick best fit
d_spec <- 1 # the number of times the data has been differenced

p_try <- seq(0,4,1)
q_try <- seq(0,4,1)
P_try <- seq(0,4,1)
Q_try <- seq(0,4,1)

# store values in list
p_lst <- list()
q_lst <- list()
P_lst <- list()
Q_lst <- list()

for (i in p_try){
  for (j in q_try){
    for (ii in P_try){
      for (jj in Q_try){
    p_lst <- append(p_lst, i)
    q_lst <- append(q_lst, j)
    P_lst <- append(P_lst, ii)
    Q_lst <- append(Q_lst, jj)
      }
    }
  }
}

pq_det <- data.frame(p=unlist(p_lst), q=unlist(q_lst), P=unlist(P_lst), Q=unlist(Q_lst))

# iterate through different model parameters for P and Q
# model the data with different model types and pick best fit
d_spec <- 1 # the number of times the data has been differenced

# store model performance results
res <- list()

for (i in seq(1,nrow(pq_det),1)){
  tryCatch({
  d_val <- d_spec
  p_val <- pq_det$p[[i]]
  q_val <- pq_det$q[[i]]
  P_val <- pq_det$P[[i]]
  Q_val <- pq_det$Q[[i]]
  
  model_test <- train %>%
    model(arima = ARIMA(Data ~ pdq(p=p_val,d=d_val,q=q_val) + PDQ(P=P_val,D=d_val,Q=Q_val)))
  
  model_test_res <- model_test %>% pivot_longer(everything(), names_to = "Model name",
                                 values_to = "Orders")
  
  # capture the model AIC and details and put in df
  model_df <- glance(model_test_res) %>% arrange(AICc) %>% select(.model:BIC)
  
  # add some identifying columns
  model_df$p_val <- p_val#pq_det$p[[i]]
  model_df$q_val <- q_val#pq_det$q[[i]]
  model_df$P_val <- P_val#pq_det$P[[i]]
  model_df$Q_val <- Q_val#pq_det$Q[[i]]
  model_df$.model <- paste("Arima(",p_val,d_val,q_val,")(",P_val,d_val,Q_val,")",sep="")
  
  res[[i]] <- model_df
  }, error = function(e) e) 
}

model_results <- do.call(rbind,res)

# sort AICc desc
model_results <- model_results %>% arrange(AICc)

# export model results
write.csv(model_results, paste(output_folder, "model_results.csv", sep = ""), row.names = FALSE)

################################################################################

# test the fitted model
model_res1 <- model_results[1:1,]
model_name_res <- model_res1$.model[[1]]

# get model to use in fit

fit_train <- train %>%
  model(arima = ARIMA(Data ~ pdq(model_res1$p_val, d_spec,model_res1$q_val)+PDQ(model_res1$P_val, d_spec,model_res1$Q_val)),
        Naive = NAIVE(Data), Snaive = SNAIVE(Data), Mean = MEAN(Data), Rw = RW(Data ~ drift()))

fit <- df_eng %>%
  model(arima = ARIMA(Data ~ pdq(model_res1$p_val, d_spec,model_res1$q_val)+PDQ(model_res1$P_val, d_spec,model_res1$Q_val)))

# check acf, residuals, ljung-box
fit_train %>% select(arima) %>% gg_tsresiduals(lag=5)
ggsave(paste(output_folder,"residuals_iter.png",sep=""),width=30,height=15,units="cm")

augment(fit_train) %>%
  filter(.model == "arima") %>%
  features(.innov, ljung_box, lag=5, dof=4) # dof degrees of freedom to match parameters of model

# get performance of model
model_perf <- accuracy(fit_train)

# export metrics
write.csv(model_perf, paste(output_folder, "model_accuracy.csv", sep=""), row.names = FALSE)

# graph these
ggplot(model_perf, aes(x=.model, y=MAPE)) +
  geom_col(fill="#1200EE") +
  geom_text(aes(label=round(MAPE,2)), vjust=-0.5) +
  labs(x="Model", y="RMSE/%", title="RMSE by model") +
  theme_classic()
ggsave(paste(output_folder,"model_rmse.png",sep=""),width=30,height=15,units="cm")

################################################################################

# forecast all models
forecast(fit_train, h=rows_to_use) %>%
  autoplot(train, level = NULL) + # NULL level to not show confidence intervals
  autolayer(test, color="black") +
  guides(colour = guide_legend(title = "Forecast")) +
  labs(title = graph_title, y="Number") +
  theme_classic()
ggsave(paste(output_folder,"forecast_all.png",sep=""),width=30,height=15,units="cm")

# forecast future values from arima model
forecast(fit, h=36) %>%
  filter(.model=='arima') %>%
  autoplot(df_eng) +
  labs(title = paste(graph_title,", ARIMA(",model_res1$p_val, d_spec,model_res1$q_val,")+(",model_res1$P_val, d_spec,model_res1$Q_val,") model", sep=""),
       y="Number") +
  theme_classic()
ggsave(paste(output_folder,"forecast_iter.png",sep=""),width=30,height=15,units="cm")

# select best model for fitting

# forecast

########### automated fitting ######################

fit <- df_eng %>%
  model(
    arima012011 = ARIMA(Data ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Data ~ pdq(2,1,0) + PDQ(0,1,1)),
    arima210000 = ARIMA(Data ~ pdq(2,1,0) + PDQ(0,0,0)),
    arima000210 = ARIMA(Data ~ pdq(0,0,0) + PDQ(2,1,0)),
    stepwise = ARIMA(Data),
    search = ARIMA(Data, stepwise=FALSE),
    auto = ARIMA(Data, stepwise = FALSE, approx = FALSE)
  )

fit %>% pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

glance(fit) %>% arrange(AICc) %>% select(.model:BIC)

################################################################################

# test the fitted model

# check acf, residulas, ljung-box

fit %>% select(arima210011) %>% gg_tsresiduals(lag=5)
ggsave(paste(output_folder,"residuals.png",sep=""),width=30,height=15,units="cm")

augment(fit) %>%
  filter(.model == "arima210011") %>%
  features(.innov, ljung_box, lag=5, dof=4) # dof degrees of freedom to match parameters of model

################################################################################

# forecast future values

forecast(fit, h=36) %>%
  filter(.model=='arima210011') %>%
  autoplot(df_eng) +
  labs(title = paste(graph_title,", ARIMA model", sep=""),
       y="Number") +
  theme_classic()
ggsave(paste(output_folder,"forecast.png",sep=""),width=30,height=15,units="cm")

################################################################################

# bootstrapping and bagging for this same data

df_stl <- df_eng %>%
  model(stl = STL(Data)) # STL or ETS

df_stl %>%
  components() %>%
  autoplot()

df_stl %>%
  generate(new_data = df_eng, times = 10,
           bootstrap_block_size = 8) %>%
  autoplot(.sim) +
  autolayer(df_eng, Data) +
  guides(colour = "none") +
  labs(title = graph_title,
       y="Number") +
  theme_classic()
ggsave(paste(output_folder,"bootstrap_generate.png",sep=""),width=30,height=15,units="cm")

sim <- df_stl %>%
  generate(new_data = df_eng, times = 100,
           bootstrap_block_size = 8) %>% # block size should represent a size of the data
  select(-.model, -Data)

ets_forecasts <- sim %>%
  model(ets = ETS(.sim)) %>%
  forecast(h = 12)

ets_forecasts %>%
  update_tsibble(key = .rep) %>%
  autoplot(.mean) +
  autolayer(df_eng, Data) +
  guides(colour = "none") +
  labs(title = paste(graph_title,": bootstrapped forecasts", sep=""),
       y="Number") +
  theme_classic()
ggsave(paste(output_folder,"bootstrap_forecast.png",sep=""),width=30,height=15,units="cm")

bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))

df_eng %>%
  model(ets = ETS(Data)) %>%
  forecast(h = 12) %>%
  autoplot(df_eng) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = paste(graph_title,", ETS model from bootstrapped averages", sep=""),
       y="Number") +
  theme_classic()
ggsave(paste(output_folder,"bagged_bootstrap_forecast.png",sep=""),width=30,height=15,units="cm")
