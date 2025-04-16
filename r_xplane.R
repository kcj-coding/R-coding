library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

folder <- "C:\\Users\\kelvi\\Desktop\\x-plane data\\"

#file <- read.csv(paste(folder, "Data.txt", sep=""))

tt <- read.table(paste(folder, "Data_a330.txt", sep=""), sep="|", header=TRUE)
tt$est <- as.numeric(rownames(tt))

# make all columns numeric
d <- list()
for (i in seq(1, length(tt),1)){
  df_chk <- tt[,i]
  df_chk <- as.numeric(as.character(gsub("\\s+","",df_chk)))
  name1 <- names(tt[i])
  # get lead and cumulative sum
  datad <- data.frame(dat=df_chk) # default is last value

  
  # rename columns
  colnames(datad) <- c(name1)
  
  d[[i]] <- datad
  
}

#dfs <- select(dfs, -testcol)
dfs <- do.call(cbind,d)

tt <- dfs

# drop NA rows from speed
tt <- tt[!is.na(tt$X_totl._time),]

#ttt <- data.frame(summary(tt))

#rename
names(tt)[names(tt) == 'p.alt.ftMSL'] <- 'X__alt.ftmsl'

# select what is wanted
cols_select <- c("X_real._time", "X_totl._time", "X_Vind._kias", "Vtrue._ktas", "X__alt.ftmsl", "pitch.__deg", "hding.__mag", "X__lat.__deg", "X__lon.__deg",
                 "X_fuel.___lb", "total.___lb", "fuel1.tankC")

tt_flt <- tt |> select(all_of(cols_select))

# order by time
tt_flt <- tt_flt[order(tt_flt$X_totl._time),]

df_chr <- tt_flt
df_chr <- df_chr[sapply(df_chr, class) == 'numeric']
ttt <- length(df_chr)
# for each column, get change from 1 row to the next and get cumulative total
# for these, make 2 new columns in df which represent the character and factor conversions
#dfs = data.frame()
#dfs <- data.frame(testcol=1:nrow(df_chr))
d <- list()
for (i in seq(1, length(df_chr),1)){
  df_chk <- df_chr[,i]
  #df_chk <- as.numeric(as.character(gsub("\\s+","",df_chk))) # check but not necessary as already numeric
  name1 <- names(df_chr[i])
  name2 <- paste(name1,"_nxt",sep="")
  name3 <- paste(name1,"_csum",sep="")
  name4 <- paste(name1,"_chg",sep="")
  name5 <- paste(name1,"_cchg",sep="")
  # get lead and cumulative sum
  datad <- data.frame(dat=df_chk, nxt=as.numeric(lead(df_chk,n=1,default=df_chk[length(df_chk)])), csum=cumsum(df_chk)) # default is last value
  #datad <- data.frame(dat=df_chk, nxt=as.numeric(lead(df_chk,n=1,default=mean(df_chk))), csum=cumsum(df_chk)) # default is mean
  datad$chg <- datad$nxt-datad$dat
  datad$cchg <- cumsum(datad$chg)
  
  # rename columns
  colnames(datad) <- c(name1,name2,name3,name4,name5)
  
  #dfs <- cbindPad(dfs,datad)
  #dfs <- cbind(dfs, datad)
  d[[i]] <- datad
  
}

#dfs <- select(dfs, -testcol)
dfs <- do.call(cbind,d)

# get altitude change in 1 sec, 1 min
dfs$altitude_chg1sec <- dfs$X__alt.ftmsl_chg/dfs$X_totl._time_chg
dfs$altitude_chg1min <- dfs$altitude_chg1sec * 60

# get distance covered in 1 sec, 1 min, 1 hr
dfs$dist_chg1sec <- dfs$Vtrue._ktas/3600#dfs$X_totl._time_chg
dfs$dist_chg1min <- dfs$dist_chg1sec * 60
dfs$dist_chg1hr <- dfs$dist_chg1min * 60

# get fuel flow burn per hour
# get total time between fuel readings (change)
# get fuel used in this time

# if fuel nxt != fuel (current) then mark as changed
dfs$ff_change <- ifelse(dfs$X_fuel.___lb_nxt != dfs$X_fuel.___lb, "changed", "no change")

# make a table of the changed times and left_join to data
dfs_ff_chg <- dfs[dfs$ff_change == "changed",]
dfs_ff_chg$ff_time_chg <- lead(dfs_ff_chg$X_real._time,1)
dfs_ff_chg$ff_time_chg <- dfs_ff_chg$ff_time_chg - dfs_ff_chg$X_real._time

dfs_ff_chg <- select(dfs_ff_chg,X_real._time,ff_time_chg)
dfs <- left_join(dfs, dfs_ff_chg, by = join_by("X_real._time"))

# divide fuel used by time, multiply by 3600 to get hourly fuel burn
dfs$ff_hr <- (abs(dfs$X_fuel.___lb_nxt - dfs$X_fuel.___lb)/dfs$ff_time_chg) * 60 * 60

tst <- dfs$X__alt.ftmsl[1]
# define flight phases based on altitude changes
flight_condition <- function(altitude,speed,altitude_chg, speed_cat, x){
  # if altitude within 50 of starting altitude and speed < speed_cat - departure
  if(altitude <= (altitude[1]+50) & speed <= speed_cat){
    #val <- "on ground"
    "on_ground"
  }
  
  # if altitude_chg (nxt) > altitude + x - climbing
  else if(altitude_chg > x){
    #val <- "climbing"
    "climbing"
  }
  
  # if altitude_chg (nxt) >= altitude - x or altitude_chg <= altitude + x - level
  else if(((altitude_chg >= 0-x) & (altitude_chg <= x))){
    #val <- "level"
    "level"
  }
  
  
  # if altitude_chg (nxt) < altitude - x - descending
  else if(altitude_chg < 0-x){
    #val <- "descending"
    "descending"
  }
  
  # if altitude within 50 of ending altitude and speed < speed_car - arrival
  else if(altitude <= (altitude[nrow(altitude)]+50) & speed <= speed_cat){
    #val <- "on ground"
    "on_ground"
  }
  
  else {
    #val <- "unknown"
    "unknown"
  }
  #return(val)
}

dfs$type <- mapply(flight_condition,dfs$X__alt.ftmsl,dfs$Vtrue._ktas,dfs$altitude_chg1min,100,50)

test <- function(x,y){
  if(x > y){
    #val <- "yes"
    "yes"
  }
  
  else{
    #val <- "no"
    "no"
  }
  #return (val)
}

dfs <- dfs |>
  #mutate(tst = test(5,10)) |>
  mutate(type = mapply(flight_condition,X__alt.ftmsl,Vtrue._ktas,altitude_chg1min,100,50))

#lm_eqn = function(df,xx,yy){
#  m = lm(yy ~ xx, df)
#  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                   list(a = format(coef(m)[1], digits = 2), 
#                        b = format(coef(m)[2], digits = 2), 
#                        r2 = format(summary(m)$r.squared, digits = 3)))
#  return(as.character(as.expression(eq)))             
#}

lm_eqn1 = function(xx,yy){
  
  m = lm(yy ~ xx)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  return(as.character(as.expression(eq)))             
}
#lm(df_nums[,j] ~ poly(df_nums[,i],3,raw=TRUE)

# lm2 model #############################################
#ap = format(unname(coef(lm2)[1]), digits = 2) # intercept
#bp = format(unname(coef(lm2)[2]), digits = 2) # bx term
#bp2 = format(unname(coef(lm2)[3]), digits = 2) # bx2 term
#bp3 = format(unname(coef(lm2)[4]), digits = 2) # bx3 term
#r2p = format(summary(lm2)$r.squared, digits = 3) # r-squared

#geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
#geom_smooth(method=lm, formula=y~splines::bs(x,3), se=FALSE, color="red")+
   
poly_eqn1 = function(xx,yy){
  
  m = lm(yy ~ poly(xx,3,raw=TRUE)) # of the form y=a+bx+b2x^2+b3x^3
  eq <- substitute(italic(y) == a + b %.% italic(x)+b2 %.% italic(x)^2+ b3 %.% italic(x)^3*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2),
                        b2 = format(coef(m)[3], digits = 2),
                        b3 = format(coef(m)[4], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  return(as.character(as.expression(eq)))             
}

#dfs$y <- dfs$Vtrue._ktas
#dfs$x <- dfs$Vtrue._ktas

#dfsxlm <- dfs |>
#  group_by(type) |>
#  summarise(eq1 = lm_eqn(dfs,dfs$y,dfs$x))

x_name <- "X_totl._time" # for graph x-axis
group_var <- "type" # for the variable to group by

# graph type by time
for (i in seq(1, length(dfs),1)){
  tst <- colnames(dfs)[i]
  df_chk <- select(dfs,!!as.name(x_name),colnames(dfs)[i], !!as.name(group_var))
  if (!(grepl(x_name,colnames(dfs)[i]) | grepl("_nxt",colnames(dfs)[i])) & is.numeric(dfs[,i])){ # if not time
    
    #xx <- select(dfs,!!as.name("X_totl._time"))
    #yy <- data.frame(select(dfs,colnames(dfs)[i]))
    
    #x_name <- !!as.name("X_totl._time")
   # y_name <- colnames(dfs)[i]
    
    dfsxlm <- df_chk |>
      group_by(!!as.name(group_var)) |>
      summarise(eq1 = lm_eqn1(!!as.name(x_name),!!as.name(colnames(dfs)[i])))
      #summarise(eq1 = lm_eqn1(df_chk[,1], df_chk[,2]))#lm_eqn(df_chk,df_chk[,1], df_chk[,2]))
    
    dfsxlmply <- df_chk |>
      group_by(!!as.name(group_var)) |>
      summarise(eq1 = poly_eqn1(!!as.name(x_name),!!as.name(colnames(dfs)[i])))
    #summarise(eq1 = lm_eqn1(df_chk[,1], df_chk[,2]))#lm_eqn(df_chk,df_chk[,1], df_chk[,2]))
    
    #lm1 <- 
    
    
    disp <- ggplot(df_chk, aes(x=!!as.name(x_name),y=!!as.name(colnames(dfs)[i]), color=!!as.name(group_var)))+
      geom_point()+
      theme_classic()
    
    disp1 <- ggplot(df_chk, aes(x=`type`,y=!!as.name(colnames(dfs)[i]), color=!!as.name(group_var)))+
      geom_boxplot()+
      theme_classic()
    
    # get equations by groupings
    
    
    disp2 <- ggplot(df_chk, aes(x=!!as.name(x_name),y=!!as.name(colnames(dfs)[i]), color=!!as.name(group_var)))+
      geom_point()+
      geom_smooth(method="lm", formula=y~x, se=FALSE, color="blue")+
      geom_smooth(method="lm", formula=y~splines::bs(x,3), se=FALSE, color="red")+
      geom_text(data=dfsxlm,aes(x = mean(select(df_chk,!!as.name(x_name))[[1]]), y = mean(select(df_chk,!!as.name(colnames(dfs)[i]))[[1]]),label=eq1), parse = TRUE, inherit.aes=FALSE, color="blue", size=4)+
      geom_text(data=dfsxlmply,aes(x = mean(select(df_chk,!!as.name(x_name))[[1]]), y = mean(select(df_chk,!!as.name(colnames(dfs)[i]))[[1]])/2,label=eq1), parse = TRUE, inherit.aes=FALSE, color="red", size=3)+
      facet_wrap(~eval(parse(text=group_var)))+
      theme_classic()
    
    # predict by type
    
    d<- list()
    #val <- nrow(unique(select(df_chk,!!as.name(group_var))))
    for (groupsi in 1:nrow(unique(select(df_chk,!!as.name(group_var))))){
      
      groups <- unique(select(df_chk,!!as.name(group_var)))#[groupsi]
      groups <- groups[groupsi,]
      
      # filter df to that group
      #df_tst <- df_chk[df_chk$type == groups,]
      df_tst <- df_chk |>
        filter(!!as.name(group_var) == groups)
      
      #dat_em <- df_tst |>
      #  group_by(zz) |>
      #  summarise(eq1 = lm_eqn(xx, yy))
      
      # lm model based on this data
      
      #yy_name <- select(df_tst,!!as.name(colnames(dfs)[i]))[[1]]
      yy_names <- as.name(colnames(dfs)[i])
      #xx_name <- select(df_tst,!!as.name("X_totl._time"))[[1]]
      xx_names <- as.name(x_name)
      
      #mdl <- lm(yy_name ~ xx_name, data=df_tst) # need to loop/filter for groups
      #mdl_ply <- lm(yy_name ~ poly(xx_name,3,raw=TRUE), data=df_tst)
      
      #df_xxx <- df_tst[,c(paste(yy_names),paste(xx_names))]
      
      mdl <- lm(as.formula(paste(yy_names,xx_names,sep="~")), data=df_tst[,c(paste(yy_names),paste(xx_names))])
      #mdl <- lm(df_tst[,1] ~ df_tst[,2], data=select(df_tst,as.name(colnames(dfs)[i]),as.name("X_totl._time")))
      #mdl <- lm(as.name(colnames(dfs)[i]) ~ as.name("X_totl._time"), data=df_tst) # need to loop/filter for groups
      #mdl_ply <- lm(yy_name ~ poly(xx_name,3,raw=TRUE))
      mdl_ply <- lm(as.formula(paste(yy_names,paste("poly(",xx_names,",3,raw=TRUE)",sep=""),sep="~")), data=df_tst[,c(paste(yy_names),paste(xx_names))])
      
      #x_rng <- unique(df$xx)
      preds_name <- paste("preds_",as.name(colnames(dfs)[i]),sep="")
      
      preds <- predict(mdl, newdata=select(df_chk,!!as.name(x_name))) # predict on all the x-axis data
      preds_ply <- predict(mdl_ply, newdata=select(df_chk,!!as.name(x_name))) # predict on all the x-axis data
      df_preds <- data.frame(xx=select(df_chk, !!as.name(x_name)),yy=preds, type=groups, model="lm")
      names(df_preds)[names(df_preds)=="xx"] <- paste(xx_names)
      names(df_preds)[names(df_preds)=="yy"] <- paste(preds_name)
      df_preds_ply <- data.frame(xx=select(df_chk, !!as.name(x_name)),yy=preds_ply, type=groups, model="ply")
      names(df_preds_ply)[names(df_preds_ply)=="xx"] <- paste(xx_names)
      names(df_preds_ply)[names(df_preds_ply)=="yy"] <- paste(preds_name)
      
      df_preds <- rbind(df_preds,df_preds_ply)
      
      d[[groupsi]] <- df_preds
      
      # disp <- ggplot(df_preds[df_preds$model=="lm",], aes(x=xx,y=preds))+#, color=zz))+
      #    geom_line()+
      #  #geom_smooth(method="loess")+
      #  geom_text(data=dat_em,aes(x = mean(df_preds$xx), y = mean(df_preds$preds),label=eq1), parse = TRUE, inherit.aes=FALSE)+
      #  facet_wrap(~zz)+
      #  theme_classic()
      
      #print(disp)
    }
    
    df_predict <- do.call(rbind,d)
    
    
    disp3 <- ggplot()+
      geom_line(data=df_predict[df_predict$model=="lm",], aes(x=!!as.name(x_name), y=!!as.name(preds_name)), color="blue", linetype = "longdash")+
      geom_line(data=df_predict[df_predict$model=="ply",], aes(x=!!as.name(x_name), y=!!as.name(preds_name)), color="red", linetype = "longdash")+
      #geom_smooth(method="loess")+
      #geom_text(data=dat_em,aes(x = mean(df_preds$xx), y = mean(df_preds$preds)*1.5,label=eq1), parse = TRUE, inherit.aes=FALSE, color="blue")+
      #geom_text(data=dat_em,aes(x = mean(df_preds$xx), y = mean(df_preds$preds)/2,label=eq2), parse = TRUE, inherit.aes=FALSE, color="red")+
      geom_point(data=df_chk, aes(x=!!as.name(x_name),y=!!as.name(colnames(dfs)[i])), color="grey")+
      facet_wrap(~eval(parse(text=group_var)))+
      theme_classic()
    
    # get variation in predictions
    df_predict_lm <- select(df_predict[df_predict$model=="lm",],!!as.name(x_name), !!as.name(group_var), paste(preds_name))
    names(df_predict_lm)[names(df_predict_lm)==preds_name] <- "preds_lm"
    colnames(df_predict_lm) <- c(x_name,group_var,"preds_lm")
    
    df_predict_ply <- select(df_predict[df_predict$model!="lm",],!!as.name(x_name), !!as.name(group_var), paste(preds_name))
    names(df_predict_ply)[names(df_predict_ply)==preds_name] <- "preds_ply"
    colnames(df_predict_ply) <- c(paste(x_name,"_1",sep=""),paste(group_var,"_1",sep=""),"preds_ply")
    
    df_compare_predicts <- cbind(df_predict_lm,df_predict_ply)
    
    var_name <- paste("var_",preds_name,sep="")
    
    df_compare_predicts <- df_compare_predicts |>
      mutate(!!as.name(var_name) := preds_lm - preds_ply)
    
    #df_compare_predicts$var <- df_compare_predicts$preds_lm - df_compare_predicts$preds_ply
    
    # add on other columns needed
    
    
    df_compare_predicts <- select(df_compare_predicts, !!as.name(x_name), !!as.name(group_var), !!as.name(var_name))
    
    disp4 <- ggplot()+
      geom_line(data=df_compare_predicts, aes(x=!!as.name(x_name), y=!!as.name(var_name)), color="black", linetype = "longdash")+
      #geom_line(data=df_predict[df_predict$model=="ply",], aes(x=!!as.name(x_name), y=!!as.name(preds_name)), color="red", linetype = "longdash")+
      #geom_smooth(method="loess")+
      #geom_text(data=dat_em,aes(x = mean(df_preds$xx), y = mean(df_preds$preds)*1.5,label=eq1), parse = TRUE, inherit.aes=FALSE, color="blue")+
      #geom_text(data=dat_em,aes(x = mean(df_preds$xx), y = mean(df_preds$preds)/2,label=eq2), parse = TRUE, inherit.aes=FALSE, color="red")+
      geom_point(color="grey")+
      facet_wrap(~eval(parse(text=group_var)))+
      theme_classic()
    
    print(disp)
    print(disp1)
    print(disp2)
    print(disp3)
    print(disp4)
  }
}

# plot route
disp <- ggplot(dfs, aes(x=`X__lon.__deg`,y=`X__lat.__deg`, color=type))+
  geom_point()+
  theme_classic()

#disp1 <- ggplot(df_chk, aes(x=`type`,y=!!as.name(colnames(dfs)[i]), color=type))+
#  geom_boxplot()+
#  theme_classic()

print(disp)
#print(disp1)

# remove all character columns from original df and add on these generated columns

# Get all numeric columns
#df <- df[sapply(df, class) == 'numeric']

# join on created data
#df <- cbind(df, dfs)


# want to see lat and long, fuel temp change over flight time
# cumulative alt change

#cor
df_cor <- cor(tt)
dfcor1 <- data.frame(row=rownames(tt)[row(tt)], col=colnames(tt)[col(tt)], corr=c(tt))
dfcor11 <- data.frame(row=colnames(tt)[col(tt)], col=rownames(tt)[row(tt)], corr=c(tt))

d2 <- tt %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1) %>%
  filter(var1 != var2)

# filter to show only those with correlations > 0.5 or < 0.5
d21 <- d2[d2$value > 0.99 | d2$value < -0.99,]
d21 <- d21[!is.na(d21$value),]
# remove sec or time
d21 <- d21[!(grepl("time",d21$var1) | grepl("sec",d21$var1)), ]
d21 <- d21[!(grepl("time",d21$var2) | grepl("sec",d21$var2)), ]
d21 <- d21[!(grepl("stick",d21$var1) | grepl("surf",d21$var1)), ]
d21 <- d21[!(grepl("stick",d21$var2) | grepl("surf",d21$var2)), ]

# plot these as scatterplot
#for (i in 1:nrow(d21)){
#  disp <- ggplot(tt, aes(!!as.name(d21$var1[i]), !!as.name(d21$var2[i]),))+
#    geom_point()
#  print(disp)
#}

# plot against time
cols_names <- c("Vtrue._ktas", "X__alt.ftmsl")
for (i in cols_names){
  disp1 <- ggplot(tt, aes(X_totl._time, !!as.name(i)))+
    geom_point()
  
  print(disp1)
}

# try lm for alt fmsl and est
df_nums <- tt
j <- "X__alt.ftmsl"
i <- "est"
df_nums <- select(df_nums, !!as.name(i),!!as.name(j))
############# linear equation examples for graph #########################
lm1 <- lm(paste0(i,"~",j), df_nums)

mod <- data.frame(vals=lm1$fitted.values, resid=lm1$residuals, actuals=df_nums[,2])

# lm1 model #############################################
a = format(unname(coef(lm1)[1]), digits = 2) # intercept
b = format(unname(coef(lm1)[2]), digits = 2) # bx term
r2 = format(summary(lm1)$r.squared, digits = 3) # r-squared

##########################################################################

################## ggplot graph ##########################################
ggplot(df_nums, aes(x=df_nums[,1], y=df_nums[,2]))+
  geom_point(alpha=0.1)+
  geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
  theme_classic()+
  labs(x=names(df_nums[1]), y=names(df_nums[2]), title=paste("Plot of ",names(df_nums[1]), " by ", names(df_nums[2]), " || LR: ", "y=",b,"x+",a," r2=",r2, sep=""))+
  theme(plot.title = element_text(size=12))

########################## lm model residuals graph ######################

ggplot(mod, aes(x=vals, y=actuals))+
  geom_point(alpha=0.1)+
  geom_smooth(method=lm, formula=y~x, se=FALSE, color="blue")+
  theme_classic()+
  labs(x=names(df_nums[1]), y=names(df_nums[2]), title=paste("Plot of residuals by ", names(df_nums[2]), " || LR: ", "y=",b,"x+",a," r2=",r2,sep=""))+
  theme(plot.title = element_text(size=12))


################################################################################

# get lat and long from position
tt$lat <- tt$X__lat.__deg#as.numeric(gsub("(,).*","",tt$Position))
tt$long <- tt$X__lon.__deg#as.numeric(gsub(".*(?<=,)", "", tt$Position, perl=TRUE))
tt$Speed <- tt$X_Vind._kias
tt$Altitude <- tt$X__alt.ftmsl

################################################################################

# plot histogram of the altitude
ggplot(tt, aes(x=Speed,y=..density..), position="identity")+
  geom_histogram(aes(y=..density..), fill="grey")+
  geom_density(alpha=0.1, linewidth=2, linetype=2)+
  geom_vline(aes(xintercept=mean(Speed), color=paste("mean = ",round(mean(Speed),2),sep="")), linetype="longdash")+
  geom_vline(aes(xintercept=median(Speed), color=paste("median = ",round(median(Speed),2),sep="")), linetype="longdash")+
  scale_color_manual(name="Legend", values=c("red","blue"))+#,values=c(!!as.name(paste("mean =",round(mean(Speed),2),sep=""))="red",!!as.name(paste("median =",round(median(Speed),2),sep=""))="blue"))+
  theme_bw()

# make a normal distribution based on this mean
x <- seq(from = mean(tt$Speed)-1*mean(tt$Speed), to = mean(tt$Speed)+1*mean(tt$Speed), by = 0.05)
norm_dat <- data.frame(x = x, pdf = dnorm(x, mean=mean(tt$Speed), sd=sd(tt$Speed)))
ggplot(norm_dat) + geom_line(aes(x = x, y = pdf))

# scatterplot speed and altitude
ggplot(tt, aes(x=Speed, y=Altitude))+
  geom_point()

# scatterplot speed and altitude
ggplot(tt, aes(x=long, y=lat))+
  geom_point()

# speed < 100 kts
speed100 <- tt[tt$Speed <= 100 & tt$Altitude > 0,]
ggplot(speed100, aes(x=Speed, y=Altitude))+
  geom_point()

ggplot(speed100, aes(x=Speed,y=..density..), position="identity")+
  geom_histogram(aes(y=..density..), fill="grey")+
  geom_density(alpha=0.1, linewidth=2, linetype=2)+
  geom_vline(aes(xintercept=mean(Speed), color=paste("mean = ",round(mean(Speed),2),sep="")), linetype=2)+
  geom_vline(aes(xintercept=median(Speed), color=paste("median = ",round(median(Speed),2),sep="")), linetype=2)+
  scale_color_manual(name="Legend", values=c("red","blue"))+#,values=c(!!as.name(paste("mean =",round(mean(Speed),2),sep=""))="red",!!as.name(paste("median =",round(median(Speed),2),sep=""))="blue"))+
  theme_bw()