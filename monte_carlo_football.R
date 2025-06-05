library(tidyverse)
library(dplyr)
library(ggplot2)
#library(summarytools)

# https://p4a.seas.gwu.edu/2023-Spring/class/13-monte-carlo-methods/#17
# https://psyteachr.github.io/data-skills-v1/simulation.html

############### configure variables ############################################

teams <- c("A","B","C","D","E")
team_probs <- c(0.1,0.4,0.9,0.7,0.3)

outcomes <- c("W","D","L")
outcome_prob <- c(0.4,0.2,0.4)
points <- c(3,1,0)

play_loc <- c("H","A")
play_prob <- c(0.6,0.4) # more chance of winning or drawing if playing at home

win_chg <- c(0.1) # winning increases probability of winning again
loss_chg <- c(-0.1) # losing increases probability of losing again

# teams play all teams twice except themselves

n_runs <- 50

goals <- c(0,1,2,3,4,5,6)
goals_prob <- c(0.5,0.4,0.3,0.2,0.1,0.03,0.01) # unlikely to score high number of goals

################################################################################

start_time <- Sys.time()

matches <- data.frame()
# make random playing order
for (h in 1:length(teams)){
  for (a in 1:length(teams)){
    if (teams[h] != teams[a]){
      match <- data.frame(home=teams[h], away=teams[a])
      matches <- rbind(matches,match)
    }}}

# shuffle this ordering
# pick random number in order and make new df
# make sure to capture all values and not to repeat any

new_order <- sample(1:nrow(matches),replace=FALSE)

matches_new <- matches[new_order,]

res <- data.frame()

#for (run in seq(1,n_runs,1)){
#  for (h in seq(1,length(teams),1)){
#    for (a in seq(1,length(teams),1)){

for (run in 1:n_runs){
  team_probs1 <- team_probs
  for (i in 1:nrow(matches_new)){
  #for (h in 1:length(teams)){
  #for (a in 1:length(teams)){
  #if (teams[h] != teams[a]){
    
    #print(teams[h]," v ", teams[a])
    #print("Home: ",teams[h], "Away: ",teams[a])
    
    # get result probability - create custom probabilities from defined starting points
    
    h_team <- matches_new$home[i]
    a_team <- matches_new$away[i]
    
    # find these entries in team_prob and update
    h_loc <- which(sapply(teams, function(y) h_team %in% y))
    a_loc <- which(sapply(teams, function(y) a_team %in% y))
    
    h_team_prob <- team_probs1[h_loc] * play_prob[1]
    #outcome_prob_h = [x*h_team_prob for x in outcome_prob]
    a_team_prob <- team_probs1[a_loc] * play_prob[2]
    
    outcome_prob_h <- c(h_team_prob,max(h_team_prob*0.2, a_team_prob*0.2),a_team_prob)
    
    result <- sample(outcomes, size=1, replace=FALSE, prob=outcome_prob_h)#random.choices(outcomes,weights=outcome_prob_h)[0]
    
    
    if (result == "W"){
      a_score = sample(goals[1:6], size=1, replace=FALSE, prob=goals_prob[1:6])
      h_score = min(6, a_score+sample(goals[2:7], size=1, replace=FALSE, prob=goals_prob[2:7]))
      # team_w prob increases
      # team_a prob decreases
      team_probs1[h_loc] <- min(0.95,(1+win_chg[1]) * team_probs[h_loc])
      team_probs1[a_loc] <- max(0.01,(1+loss_chg[1]) * team_probs[a_loc])
    }
    else if (result == "L"){
      a_score = sample(goals[2:7], size=1, replace=FALSE, prob=goals_prob[2:7])
      h_score = max(0, a_score-sample(goals[1:6], size=1, replace=FALSE, prob=goals_prob[1:6]))
      # team_w prob decreases
      # team_a prob increases
      team_probs1[h_loc] <- max(0.01,(1+loss_chg[1]) * team_probs[h_loc])
      team_probs1[a_loc] <- min(0.95,(1+win_chg[1]) * team_probs[a_loc])
    }
    else{
      a_score <- sample(goals[1:5], size=1, replace=FALSE, prob=goals_prob[1:5])
      h_score <- a_score
    }
    
    exp_result <- ifelse(h_team_prob > a_team_prob, "W", "L")
    
    h_points <- ifelse(result=="W", 3, ifelse(result=="L",0,1))
    a_points <- ifelse(result=="W", 0, ifelse(result=="L",3,1))
    
    #print(result)
    
    # outcome probability
    dff <- data.frame(run=run, home=teams[h_loc], away=teams[a_loc], probs=paste(paste(outcome_prob_h,sep=" "),collapse=''), result=result, score=paste(h_score,":",a_score,sep=""),goals_h=h_score, goals_a=a_score, exp_result=exp_result,
                      h_prob=team_probs1[h_loc], a_prob=team_probs1[a_loc], h_points=h_points, a_points=a_points)
    
    res <- rbind(res,dff)
    
    
  }}#}}

# at the end, want to track team positions by run
df_teams <- data.frame()

for (team in teams){
  dfi <- res[((res$home==team) | (res$away==team)),]
  
  dfi_h <- res[res$home==team,]
  dfi_a <- res[res$away==team,]
  
  # want to know run, team, and total number of h_points
  dfi_hi <- dfi_h |>
    group_by(run,home) |>
    summarise(sum(h_points))
  
  colnames(dfi_hi) <- c("run","team","points")
  
  # want to know run, team, and total number of a_points
  dfi_ai <- dfi_a |>
    group_by(run,away) |>
    summarise(sum(a_points))
  
  colnames(dfi_ai) <- c("run","team","points")
  
  # add points together and add to df run, team, total points
  df_xx <- rbind(dfi_hi,dfi_ai)
  
  df_xx <- df_xx |>
    group_by(run,team) |>
    summarise(sum(points))
  
  colnames(df_xx) <- c("run","team","points")
  
  # get goals for, goals against and goal difference
  df_goals_for_h <- dfi_h |>
    group_by(run) |>
    summarise(sum(goals_h))
  
  colnames(df_goals_for_h) <- c("run", "goals_for")
  
  df_goals_for_a <- dfi_a |>
    group_by(run) |>
    summarise(sum(goals_a))
  
  colnames(df_goals_for_a) <- c("run", "goals_for")
  
  df_goals_for = rbind(df_goals_for_h, df_goals_for_a)
  df_goals_for <- df_goals_for |> group_by(run) |> summarise(sum(goals_for))
  
  colnames(df_goals_for) <- c("run", "goals_for")
  
  # goals against
  df_goals_agn_h <- dfi_h |>
    group_by(run) |>
    summarise(sum(goals_a))
  
  colnames(df_goals_agn_h) <- c("run", "goals_agn")
  
  df_goals_agn_a <- dfi_a |>
    group_by(run) |>
    summarise(sum(goals_h))
  
  colnames(df_goals_agn_a) <- c("run", "goals_agn")
  
  df_goals_agn = rbind(df_goals_agn_h, df_goals_agn_a)
  df_goals_agn <- df_goals_agn |> group_by(run) |> summarise(sum(goals_agn))
  
  colnames(df_goals_agn) <- c("run", "goals_agn")
  
  # combine
  df_xx <- left_join(df_xx, df_goals_for, by=c("run"="run"))
  df_xx <- left_join(df_xx, df_goals_agn, by=c("run"="run"))
  df_xx$goal_diff <- df_xx$goals_for - df_xx$goals_agn
  
  df_teams <- rbind(df_teams, df_xx)
  
}

# then at end rank all teams in each run based on total points
df_teams <- df_teams[order(df_teams$run, -df_teams$points, df_teams$goal_diff),]

df_teams$rank <- 1

df_teams <- df_teams |> group_by(run) |> mutate(rank=cumsum(rank)) |> ungroup()

# finally, for each team want to know the proportion percentage of time finishing at each position
# get the final positions for each team and calculate number of times (out of total) at each position
df_teams_f <- df_teams |> group_by(team,rank) |> summarise(n())

# want to make df that has all possible positions, and join for each team
# fill in any missing or NA values with 0
dff <- data.frame(rank=1:length(teams))

df_teams_f <- left_join(dff, df_teams_f, by=c("rank"="rank"))
colnames(df_teams_f) <- c("rank","team","points")

df_teams_f <- df_teams_f |> group_by(team) |> mutate(pct=(points/sum(points))) |> ungroup()

# pivot longer for rank
dff_lng <- df_teams_f |> select(team, rank, pct) |> pivot_wider(names_from = rank, values_from = pct, values_fill=0)
#dff_lng <- dff_lng |> replace_na(0)
dff_lng <- dff_lng[order(dff_lng$team),]

df_teams_f1 <- df_teams_f

df_teams_f1$team <- factor(df_teams_f1$team, levels = c(sort(unique(df_teams_f1$team), decreasing = TRUE)))

# make heatmap
ggplot(df_teams_f1, aes(x=rank, y=team, fill=pct))+
  geom_tile()+
  geom_text(aes(label=pct))+
  scale_fill_gradient(low="yellow",high="red")+
  labs(x="Rank", y="Team", title="Title")+
  theme_classic()

#save graph
#ggsave(path="C:\\","file.png",height=15,width=30,units="cm")


end_time <- Sys.time()
print(paste("runtime: ", end_time-start_time, sep=""))