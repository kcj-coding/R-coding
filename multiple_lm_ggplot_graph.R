library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)


lm_eqn1 = function(xx,yy){
  
  m = lm(yy ~ xx)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  return(as.character(as.expression(eq)))             
}

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

################ create df ################################################################

df <- data.frame(xx=rnorm(200), yy=rnorm(200), zz=rep(c("a","b", "c", "d"),times=50))
df <- data.frame(xx=seq(1,200,1), yy=rep(c(1,2,3,2),each=50), zz=rep(c("a","b", "c", "d"),times=50))

df_x <- data.frame(xx=rnorm(400), zz=rep(c("a","b", "c", "d"),times=100))
df_x <- data.frame(xx=seq(1,400,1), zz=rep(c("a","b", "c", "d"),times=100))


############### graph the data and lm #######################

x_name <- "xx" # for graph x-axis
group_var <- "zz" # for the variable to group by

dfs <- df

# graph type by time
for (i in seq(1, length(dfs),1)){
  tst <- colnames(dfs)[i]
  df_chk <- select(dfs,!!as.name(x_name),colnames(dfs)[i], !!as.name(group_var))
  if (!(grepl(x_name,colnames(dfs)[i]) | grepl("_nxt",colnames(dfs)[i])) & is.numeric(dfs[,i])){ # if not time
    
    
    dfsxlm <- df_chk |>
      group_by(!!as.name(group_var)) |>
      summarise(eq1 = lm_eqn1(!!as.name(x_name),!!as.name(colnames(dfs)[i])))

    
    dfsxlmply <- df_chk |>
      group_by(!!as.name(group_var)) |>
      summarise(eq1 = poly_eqn1(!!as.name(x_name),!!as.name(colnames(dfs)[i])))
    
    
    disp <- ggplot(df_chk, aes(x=!!as.name(x_name),y=!!as.name(colnames(dfs)[i]), color=!!as.name(group_var)))+
      geom_point()+
      theme_classic()
    
    disp1 <- ggplot(df_chk, aes(x=!!as.name(group_var),y=!!as.name(colnames(dfs)[i]), color=!!as.name(group_var)))+
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

    for (groupsi in 1:nrow(unique(select(df_chk,!!as.name(group_var))))){
      
      groups <- unique(select(df_chk,!!as.name(group_var)))#[groupsi]
      groups <- groups[groupsi,]
      
      # filter df to that group
      #df_tst <- df_chk[df_chk$type == groups,]
      df_tst <- df_chk |>
        filter(!!as.name(group_var) == groups)
      
      # lm model based on this data
      

      yy_names <- as.name(colnames(dfs)[i])

      xx_names <- as.name(x_name)
      
      
      mdl <- lm(as.formula(paste(yy_names,xx_names,sep="~")), data=df_tst[,c(paste(yy_names),paste(xx_names))])

      mdl_ply <- lm(as.formula(paste(yy_names,paste("poly(",xx_names,",3,raw=TRUE)",sep=""),sep="~")), data=df_tst[,c(paste(yy_names),paste(xx_names))])
      
      #x_rng <- unique(df$xx)
      preds_name <- paste("preds_",as.name(colnames(dfs)[i]),sep="")
      
      preds <- predict(mdl, newdata=select(df_chk,!!as.name(x_name))) # predict on all the x-axis data
      preds_ply <- predict(mdl_ply, newdata=select(df_chk,!!as.name(x_name))) # predict on all the x-axis data
      df_preds <- data.frame(xx=select(df_chk, !!as.name(x_name)),yy=preds, type=groups, model="lm")
      names(df_preds)[names(df_preds)=="xx"] <- paste(xx_names)
      names(df_preds)[names(df_preds)=="yy"] <- paste(preds_name)
      names(df_preds)[names(df_preds)=="type"] <- paste(group_var)
      df_preds_ply <- data.frame(xx=select(df_chk, !!as.name(x_name)),yy=preds_ply, type=groups, model="ply")
      names(df_preds_ply)[names(df_preds_ply)=="xx"] <- paste(xx_names)
      names(df_preds_ply)[names(df_preds_ply)=="yy"] <- paste(preds_name)
      names(df_preds_ply)[names(df_preds_ply)=="type"] <- paste(group_var)
      
      df_preds <- rbind(df_preds,df_preds_ply)
      
      d[[groupsi]] <- df_preds
      
    }
    
    df_predict <- do.call(rbind,d)
    
    
    disp3 <- ggplot()+
      geom_line(data=df_predict[df_predict$model=="lm",], aes(x=!!as.name(x_name), y=!!as.name(preds_name)), color="blue", linetype = "longdash")+
      geom_line(data=df_predict[df_predict$model=="ply",], aes(x=!!as.name(x_name), y=!!as.name(preds_name)), color="red", linetype = "longdash")+
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

    # add on other columns needed
    
    df_compare_predicts <- select(df_compare_predicts, !!as.name(x_name), !!as.name(group_var), !!as.name(var_name))
    
    disp4 <- ggplot()+
      geom_line(data=df_compare_predicts, aes(x=!!as.name(x_name), y=!!as.name(var_name)), color="black", linetype = "longdash")+
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
