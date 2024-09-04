library(dplyr)
library(fpp3)
# https://otexts.com/fpp3/
library(ggplot2)

output_folder <- "C:\\Users\\kelvi\\Desktop\\"

graph_title = "Number by day UK"

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
  labs(x="Day", y="Number", title=graph_title)
ggsave(paste(output_folder,"basic_graph1.png",sep=""),width=30,height=15,units="cm")

#dev.print(png,paste(output_folder,"chart.png",sep=""),width=1920,height=1080)
autoplot(df_eng,Data) +
  labs(title = graph_title,
       y="Number")
ggsave(paste(output_folder,"basic_graph2.png",sep=""),width=30,height=15,units="cm")

# check for differences, make sure stationary
df_eng %>%
  gg_tsdisplay(difference(Data, 1),
               plot_type='partial', lag=5) +
  labs(title="Seasonally differenced", y="")
ggsave(paste(output_folder,"differences1.png",sep=""),width=30,height=15,units="cm")

# difference again
df_eng %>%
  gg_tsdisplay(difference(Data, 1) |> difference(),
               plot_type='partial', lag=5) +
  labs(title = "Double differenced", y="")
ggsave(paste(output_folder,"differences2.png",sep=""),width=30,height=15,units="cm")

# model the data with different model types and pick best fit

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

# check acf, residulas, ljung-box

fit %>% select(arima210011) %>% gg_tsresiduals(lag=5)

augment(fit) %>%
  filter(.model == "arima210011") %>%
  features(.innov, ljung_box, lag=5, dof=4) #dof degrees of freedom to match parameters of model

# forecast future values


forecast(fit, h=36) %>%
  filter(.model=='arima210011') %>%
  autoplot(df_eng) +
  labs(title = paste(graph_title,", ARIMA model", sep=""),
       y="Number")
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
       y="Number")
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
       y="Number")
ggsave(paste(output_folder,"bootstrap_forecast.png",sep=""),width=30,height=15,units="cm")

bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))

df_eng %>%
  model(ets = ETS(Data)) %>%
  forecast(h = 12) %>%
  autoplot(df_eng) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = paste(graph_title,", ETS model from bootstrapped averages", sep=""),
       y="Number")
ggsave(paste(output_folder,"bagged_bootstrap_forecast.png",sep=""),width=30,height=15,units="cm")
