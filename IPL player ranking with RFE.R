#Loading libraries
#install.packages('tidyverse')
#install.packages('plyr')
#install.packages('data.table')
#install.packages('mlbench')
#install.packages('caret')
#install.packages('
library(tidyverse) 
library(plyr)
library(data.table)
library(mlbench)
library(caret)
library(randomForest)

#Setting up path where datasets are located
setwd("C:\\Users\\rohan\\Desktop\\Praxis coureswork\\Term 2\\R programming\\ipl")

#Reading the datasets
df_ballbyball <- read.csv("Ball_By_Ball.csv")
#df_match <- read.csv("Match.csv")
df_player <- read.csv("Player.csv")
#df_playermatch <- read.csv("Player_match.csv")
#df_team <- read.csv("Team.csv")

#Check for duplicates in players column
length(unique(df_player$Player_Name)) == length(df_player$Player_Name)

#Adding player id and player name to new dataframe
df_master = data.frame('Player_Id' = df_player$Player_Id, 'Player_Name' = df_player$Player_Name)


#--------------------------------------------------------
#Total runs scored by a player
season_ballbyball=subset(df_ballbyball,df_ballbyball$Season==2017 | df_ballbyball$Season==2016 | df_ballbyball$Season==2015)
res = ddply(season_ballbyball, .(Striker,Season), function(x) return(c(Runs = sum(x$Runs_Scored))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
df_masterOut <- merge(x = df_master, y = res, by = "Player_Id", all.x = TRUE)
df_masterOut <- merge(x = df_master, y = res, by = "Player_Id", all.x = TRUE)

#Balls played by a batsman
res = ddply(season_ballbyball, .(Striker,Season), function(x) return(c(BallsFaced = length(x$Runs_Scored))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#Out innings
res = ddply(season_ballbyball, .(Striker,Season,MatcH_id), function(x) return(c(OutOrNot = sum(x$Bowler_Wicket))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
res = ddply(res, .(Player_Id,Season), function(x) return(c(Out_count = sum(x$OutOrNot))))
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#RunOut innings
res = ddply(season_ballbyball, .(Striker,Season,MatcH_id), function(x) return(c(OutOrNot = sum(x$Run_out))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
res = ddply(res, .(Player_Id,Season), function(x) return(c(RunOut_count = sum(x$OutOrNot))))
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#Total innings played
res = ddply(season_ballbyball, .(Striker,Season), function(x) return(c(NumberOfInnings = length(unique(x$MatcH_id)))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#4s hit
res = ddply(season_ballbyball, .(Striker, Season), function(x) return(c(`4s` = sum(x$Runs_Scored == 4))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#6s hit
res = ddply(season_ballbyball, .(Striker, Season), function(x) return(c(`6s` = sum(x$Runs_Scored == 6))))
colnames(res)[which(names(res) == "Striker")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#4 wickets
res = ddply(season_ballbyball, .(Bowler, Season, MatcH_id), function(x) return(c(`4wicks` = sum(x$Bowler_Wicket))))
colnames(res)[which(names(res) == "Bowler")] <- "Player_Id"
res = ddply(res, .(Player_Id,Season), function(x) return(c(`4wicks` = sum(x$`4wicks` == 4))))
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#5 wickets
res = ddply(season_ballbyball, .(Bowler, Season, MatcH_id), function(x) return(c(`5wicks` = sum(x$Bowler_Wicket))))
colnames(res)[which(names(res) == "Bowler")] <- "Player_Id"
res = ddply(res, .(Player_Id,Season), function(x) return(c(`5wicks` = sum(x$`5wicks` == 5))))
df_masterOut <- merge(x = df_masterOut, y = res, by =c("Player_Id","Season"), all.x = TRUE)

#Runs given by a player/bowler
res = ddply(season_ballbyball, .(Bowler, Season), function(x) return(c(runs_given = sum(x[,c(9:16)]))))
colnames(res)[which(names(res) == "Bowler")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by = c("Player_Id","Season"), all.x = TRUE)

#Balls bowled by player/bowler
res = ddply(season_ballbyball, .(Bowler, Season), function(x) return(c(balls_bowled = nrow(x))))
colnames(res)[which(names(res) == "Bowler")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by = c("Player_Id","Season"), all.x = TRUE)

#Number of wickets taken
res = ddply(season_ballbyball, .(Bowler, Season), function(x) return(c(Wickets_taken = sum(x$Bowler_Wicket))))
colnames(res)[which(names(res) == "Bowler")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by = c("Player_Id","Season"), all.x = TRUE)

#Number of innings bowled
res = ddply(season_ballbyball, .(Bowler, Season), function(x) return(c(No_inn_bowld = length(unique(x$MatcH_id)))))
colnames(res)[which(names(res) == "Bowler")] <- "Player_Id"
df_masterOut <- merge(x = df_masterOut, y = res, by = c("Player_Id","Season"), all.x = TRUE)

####Merging data to a master table#####
df2017=subset(df_masterOut,Season==2017)
df2016=subset(df_masterOut,Season==2016)
df2015=subset(df_masterOut,Season==2015)

#Setting column names according to seasons
setnames(df2015, old=c("Runs","BallsFaced","Out_count","RunOut_count","NumberOfInnings","4s","6s","4wicks","5wicks","runs_given","balls_bowled","Wickets_taken","No_inn_bowld"), new=c("Runs_2015","BallsFaced_2015","Out_count_2015","RunOut_count_2015","NumberOfInnings_2015","4s_2015","6s_2015","4wicks_2015","5wicks_2015","runs_given_2015","balls_bowled_2015","Wickets_taken_2015","No_inn_bowld_2015"))
setnames(df2016, old=c("Runs","BallsFaced","Out_count","RunOut_count","NumberOfInnings","4s","6s","4wicks","5wicks","runs_given","balls_bowled","Wickets_taken","No_inn_bowld"), new=c("Runs_2016","BallsFaced_2016","Out_count_2016","RunOut_count_2016","NumberOfInnings_2016","4s_2016","6s_2016","4wicks_2016","5wicks_2016","runs_given_2016","balls_bowled_2016","Wickets_taken_2016","No_inn_bowld_2016"))
setnames(df2017, old=c("Runs","BallsFaced","Out_count","RunOut_count","NumberOfInnings","4s","6s","4wicks","5wicks","runs_given","balls_bowled","Wickets_taken","No_inn_bowld"), new=c("Runs_2017","BallsFaced_2017","Out_count_2017","RunOut_count_2017","NumberOfInnings_2017","4s_2017","6s_2017","4wicks_2017","5wicks_2017","runs_given_2017","balls_bowled_2017","Wickets_taken_2017","No_inn_bowld_2017"))

#Merging datasets
df_masterFinal <- merge(x = df_master, y = df2015, by =c("Player_Id"), all.x = TRUE)
df_masterFinal <- merge(x = df_masterFinal, y = df2016, by =c("Player_Id"), all.x = TRUE)
df_masterFinal <- merge(x = df_masterFinal, y = df2017, by =c("Player_Id"), all.x = TRUE)

df_masterFinal <- df_masterFinal[ , -which(names(df_masterFinal) %in% c("Season.x","Player_Name.x","Season.y","Player_Name.y","Season","Player_Name"))]

#Setting na's to zero for modelling purposes
df_masterFinal[is.na(df_masterFinal)] <- 0

df_ipl = df_masterFinal

#Creating cumulative columns
df_ipl$`4s_17_16` <- df_ipl$`4s_2017` + df_ipl$`4s_2016`
df_ipl$`4s_17_16_15` <- df_ipl$`4s_2017` + df_ipl$`4s_2016` + df_ipl$`4s_2015`
df_ipl$`6s_17_16` <- df_ipl$`6s_2017` + df_ipl$`6s_2016`
df_ipl$`6s_17_16_15` <- df_ipl$`6s_2017` + df_ipl$`6s_2016` + df_ipl$`6s_2015`

df_ipl$`4wicks_17_16` <- df_ipl$`4wicks_2017` + df_ipl$`4wicks_2016`
df_ipl$`4wicks_17_16_15` <- df_ipl$`4wicks_2017` + df_ipl$`4wicks_2016` + df_ipl$`4wicks_2015`
df_ipl$`5wicks_17_16` <- df_ipl$`5wicks_2017` + df_ipl$`5wicks_2016`
df_ipl$`5wicks_17_16_15` <- df_ipl$`5wicks_2017` + df_ipl$`5wicks_2016` + df_ipl$`5wicks_2015`

df_ipl$runs_given_17_16 <- df_ipl$runs_given_2017 + df_ipl$runs_given_2016
df_ipl$runs_given_17_16_15 <- df_ipl$runs_given_2017 + df_ipl$runs_given_2016 + df_ipl$runs_given_2015

df_ipl$balls_bowled_17_16 <- df_ipl$balls_bowled_2017 + df_ipl$balls_bowled_2016
df_ipl$balls_bowled_17_16_15 <- df_ipl$balls_bowled_2017 + df_ipl$balls_bowled_2016 + df_ipl$balls_bowled_2015

df_ipl$Wickets_taken_17_16 <- df_ipl$Wickets_taken_2017 + df_ipl$Wickets_taken_2016
df_ipl$Wickets_taken_17_16_15 <- df_ipl$Wickets_taken_2017 + df_ipl$Wickets_taken_2016 + df_ipl$Wickets_taken_2015

df_ipl$No_inn_bowld_17_16 <- df_ipl$No_inn_bowld_2017 + df_ipl$No_inn_bowld_2016
df_ipl$No_inn_bowld_17_16_15 <- df_ipl$No_inn_bowld_2017 + df_ipl$No_inn_bowld_2016 + df_ipl$No_inn_bowld_2015

df_ipl$runs_given_17_16 <- df_ipl$runs_given_2017 + df_ipl$runs_given_2016
df_ipl$runs_given_17_16_15 <- df_ipl$runs_given_2017 + df_ipl$runs_given_2016 + df_ipl$runs_given_2015

df_ipl$Runs_17_16 <- df_ipl$Runs_2017 + df_ipl$Runs_2016
df_ipl$Runs_17_16_15 <- df_ipl$Runs_2017 + df_ipl$Runs_2016 + df_ipl$Runs_2015

df_ipl$BallsFaced_17_16 <- df_ipl$BallsFaced_2017 + df_ipl$BallsFaced_2016
df_ipl$BallsFaced_17_16_15 <- df_ipl$BallsFaced_2017 + df_ipl$BallsFaced_2016 + df_ipl$BallsFaced_2015

df_ipl$Out_count_17_16 <- df_ipl$Out_count_2017 + df_ipl$Out_count_2016
df_ipl$Out_count_17_16_15 <- df_ipl$Out_count_2017 + df_ipl$Out_count_2016 + df_ipl$Out_count_2015

df_ipl$RunOut_count_17_16 <- df_ipl$RunOut_count_2017 + df_ipl$RunOut_count_2016
df_ipl$RunOut_count_17_16_15 <- df_ipl$RunOut_count_2017 + df_ipl$RunOut_count_2016 + df_ipl$RunOut_count_2015

df_ipl$NumberOfInnings_17_16 <- df_ipl$NumberOfInnings_2017 + df_ipl$NumberOfInnings_2016
df_ipl$NumberOfInnings_17_16_15 <- df_ipl$NumberOfInnings_2017 + df_ipl$NumberOfInnings_2016 + df_ipl$NumberOfInnings_2015


#Removing incorrect data
df_ipl$RunOut_count_2017[df_ipl$NumberOfInnings_2017 < (df_ipl$Out_count_2017 + df_ipl$RunOut_count_2017)] <- 0
df_ipl$RunOut_count_2016[df_ipl$NumberOfInnings_2016 < (df_ipl$Out_count_2016 + df_ipl$RunOut_count_2016)] <- 0
df_ipl$RunOut_count_2015[df_ipl$NumberOfInnings_2015 < (df_ipl$Out_count_2015 + df_ipl$RunOut_count_2015)] <- 0



df_ipl_metric <- data.frame('Player_Id' = df_player$Player_Id, 'Player_Name' = df_player$Player_Name)

#Batting factors----------------
#####2017
#1.Hard Hitting Ability = (4*Fours + 6*Sixes) / Balls Played by Batsman
df_ipl_metric$hard_Hitting_Ability_17 <- (4*(df_ipl$`4s_2017`) + 6*(df_ipl$`6s_2017`))/df_ipl$BallsFaced_2017

#2.Finisher = Not Out innings / Total Innings played
df_ipl_metric$Finisher_17 <- (df_ipl$NumberOfInnings_2017-df_ipl$Out_count_2017-df_ipl$RunOut_count_2017)/df_ipl$NumberOfInnings_2017

#3.Fast Scoring Ability = Total Runs / Balls Played by Batsman
df_ipl_metric$Fast_Scoring_ability_17 <- df_ipl$Runs_2017/df_ipl$BallsFaced_2017

#4.Consistency = Total Runs/Number of Times Out
df_ipl_metric$Consistency_bat_17 <- df_ipl$Runs_2017/df_ipl$Out_count_2017

#5.Running Between Wickets = (Total Runs - (4*Fours + 6*Sixes))/(Total Balls Played - Boundary Balls)
df_ipl_metric$RunsBetweenWicket_17 <- (df_ipl$Runs_2017-(4*(df_ipl$`4s_2017`) + 6*(df_ipl$`6s_2017`)))/(df_ipl$BallsFaced_2017-(df_ipl$`4s_2017`+df_ipl$`6s_2017`))

#####2017 - 2016
#1.Hard Hitting Ability = (4*Fours + 6*Sixes) / Balls Played by Batsman
df_ipl_metric$hard_Hitting_Ability_17_16 <- (4*(df_ipl$`4s_17_16`) + 6*(df_ipl$`4s_17_16`))/df_ipl$BallsFaced_17_16

#2.Finisher = Not Out innings / Total Innings played
df_ipl_metric$Finisher_17_16 <- (df_ipl$NumberOfInnings_17_16-df_ipl$Out_count_17_16-df_ipl$RunOut_count_17_16)/df_ipl$NumberOfInnings_17_16

#3.Fast Scoring Ability = Total Runs / Balls Played by Batsman
df_ipl_metric$Fast_Scoring_ability_17_16 <- df_ipl$Runs_17_16/df_ipl$BallsFaced_17_16

#4.Consistency = Total Runs/Number of Times Out
df_ipl_metric$Consistency_bat_17_16 <- df_ipl$Runs_17_16/df_ipl$Out_count_17_16

#5.Running Between Wickets = (Total Runs - (4*Fours + 6*Sixes))/(Total Balls Played - Boundary Balls)
df_ipl_metric$RunsBetweenWicket_17_16 <- (df_ipl$Runs_17_16-(4*(df_ipl$`4s_17_16`) + 6*(df_ipl$`4s_17_16`)))/(df_ipl$BallsFaced_17_16-(df_ipl$`4s_17_16`+df_ipl$`4s_17_16`))


#####2017 - 2015
#1.Hard Hitting Ability = (4*Fours + 6*Sixes) / Balls Played by Batsman
df_ipl_metric$hard_Hitting_Ability_17_16_15 <- (4*(df_ipl$`4s_17_16_15`) + 6*(df_ipl$`4s_17_16_15`))/df_ipl$BallsFaced_17_16_15

#2.Finisher = Not Out innings / Total Innings played
df_ipl_metric$Finisher_17_16_15 <- (df_ipl$NumberOfInnings_17_16_15-df_ipl$Out_count_17_16_15-df_ipl$RunOut_count_17_16_15)/df_ipl$NumberOfInnings_17_16_15

#3.Fast Scoring Ability = Total Runs / Balls Played by Batsman
df_ipl_metric$Fast_Scoring_ability_17_16_15 <- df_ipl$Runs_17_16_15/df_ipl$BallsFaced_17_16_15

#4.Consistency = Total Runs/Number of Times Out
df_ipl_metric$Consistency_bat_17_16_15 <- df_ipl$Runs_17_16_15/df_ipl$Out_count_17_16_15

#5.Running Between Wickets = (Total Runs - (4*Fours + 6*Sixes))/(Total Balls Played - Boundary Balls)
df_ipl_metric$RunsBetweenWicket_17_16_15 <- (df_ipl$Runs_17_16_15-(4*(df_ipl$`4s_17_16_15`) + 6*(df_ipl$`4s_17_16_15`)))/(df_ipl$BallsFaced_17_16_15-(df_ipl$`4s_17_16_15`+df_ipl$`4s_17_16_15`))

#Bowling factors----------------
#####2017
#1.Economy = Runs Scored / (Number of balls bowled by bowler/6)
df_ipl_metric$Economy_17=(df_ipl$runs_given_2017)/(df_ipl$balls_bowled_2017/6)

#2.Wicket Taking Ability = Number of balls bowled / Wickets Taken
df_ipl_metric$Wicket_Taking_Ability_17=df_ipl$balls_bowled_2017/df_ipl$Wickets_taken_2017

#3.Consistency = Runs Conceded / Wickets Taken
df_ipl_metric$Ball_Consistency_17=df_ipl$runs_given_2017/df_ipl$Wickets_taken_2017

#4.Crucial Wicket Taking Ability = Number of times Four or Five Wickets Taken / Number of Innings Played
df_ipl_metric$Crucial_Wicket_Taking_Ability_17=(df_ipl$`4wicks_2017`+df_ipl$`5wicks_2017`)/df_ipl$No_inn_bowld_2017

#5.Short Performance Index = (Wickets Taken - 4* Number of Times Four Wickets Taken - 5* Number of Times Five Wickets Taken) 
#/ (Innings Played - Number of Times Four Wickets or Five Wickets Taken)
df_ipl_metric$Short_Performance_Index_17=(df_ipl$Wickets_taken_2017-4*df_ipl$`4wicks_2017`-5*df_ipl$`5wicks_2017`)/(df_ipl$No_inn_bowld_2017-df_ipl$`4wicks_2017`-df_ipl$`5wicks_2017`)

#####2017 - 2016
#1.Economy = Runs Scored / (Number of balls bowled by bowler/6)
df_ipl_metric$Economy_17_16=(df_ipl$runs_given_17_16)/(df_ipl$balls_bowled_17_16/6)

#2.Wicket Taking Ability = Number of balls bowled / Wickets Taken
df_ipl_metric$Wicket_Taking_Ability_17_16=df_ipl$balls_bowled_17_16/df_ipl$Wickets_taken_17_16

#3.Consistency = Runs Conceded / Wickets Taken
df_ipl_metric$Ball_Consistency_17_16=df_ipl$runs_given_17_16/df_ipl$Wickets_taken_17_16

#4.Crucial Wicket Taking Ability = Number of times Four or Five Wickets Taken / Number of Innings Played
df_ipl_metric$Crucial_Wicket_Taking_Ability_17_16=(df_ipl$`4wicks_17_16`+df_ipl$`5wicks_17_16`)/df_ipl$No_inn_bowld_17_16

#5.Short Performance Index = (Wickets Taken - 4* Number of Times Four Wickets Taken - 5* Number of Times Five Wickets Taken) 
#/ (Innings Played - Number of Times Four Wickets or Five Wickets Taken)
df_ipl_metric$Short_Performance_Index_17_16=(df_ipl$Wickets_taken_17_16-4*df_ipl$`4wicks_17_16`-5*df_ipl$`5wicks_17_16`)/(df_ipl$No_inn_bowld_17_16-df_ipl$`4wicks_17_16`-df_ipl$`5wicks_17_16`)


#####2017 - 2015
#1.Economy = Runs Scored / (Number of balls bowled by bowler/6)
df_ipl_metric$Economy_17_16_15=(df_ipl$runs_given_17_16_15)/(df_ipl$balls_bowled_17_16_15/6)

#2.Wicket Taking Ability = Number of balls bowled / Wickets Taken
df_ipl_metric$Wicket_Taking_Ability_17_16_15=df_ipl$balls_bowled_17_16_15/df_ipl$Wickets_taken_17_16_15

#3.Consistency = Runs Conceded / Wickets Taken
df_ipl_metric$Ball_Consistency_17_16_15=df_ipl$runs_given_17_16_15/df_ipl$Wickets_taken_17_16_15

#4.Crucial Wicket Taking Ability = Number of times Four or Five Wickets Taken / Number of Innings Played
df_ipl_metric$Crucial_Wicket_Taking_Ability_17_16_15=(df_ipl$`4wicks_17_16_15`+df_ipl$`5wicks_17_16_15`)/df_ipl$No_inn_bowld_17_16_15

#5.Short Performance Index = (Wickets Taken - 4* Number of Times Four Wickets Taken - 5* Number of Times Five Wickets Taken) 
#/ (Innings Played - Number of Times Four Wickets or Five Wickets Taken)
df_ipl_metric$Short_Performance_Index_17_16_15=(df_ipl$Wickets_taken_17_16_15-4*df_ipl$`4wicks_17_16_15`-5*df_ipl$`5wicks_17_16_15`)/(df_ipl$No_inn_bowld_17_16_15-df_ipl$`4wicks_17_16_15`-df_ipl$`5wicks_17_16_15`)

df_ipl_metric[mapply(is.infinite, df_ipl_metric)] <- NA
df_ipl_metric[ is.na(df_ipl_metric) ] <- NA

df_ipl_metric <- df_ipl_metric[!is.na(df_ipl_metric[,c(3:ncol(df_ipl_metric))]),]

df_ipl_metric <- df_ipl_metric[complete.cases(df_ipl_metric[ , 1]),]


df_merged <- merge(x = df_ipl_metric, y = df_ipl, by = c("Player_Id"), all.x = TRUE)

#Creating metrics to calculate MVPI
TBA <- sum(df_merged$Runs_2017)/sum(df_merged$Wickets_taken_2017)
TBSR <- sum(df_merged$Runs_2017)/sum(df_merged$BallsFaced_2017)
df_merged$Batting_points_17 <- (df_merged$Consistency_bat_17/TBA)*df_merged$Runs_2017 + (df_merged$Fast_Scoring_ability_17/TBSR)*df_merged$Runs_2017

TBB <- sum(df_merged$balls_bowled_2017)
TWT <- sum(df_merged$Wickets_taken_2017)
TBWA <- TBB/TWT
TRC <- sum(df_merged$runs_given_2017)
TBWER <- (TRC*6)/TBB
PBWER <- (df_merged$runs_given_2017*6)/df_merged$balls_bowled_2017
PWT <- df_merged$Wickets_taken_2017
PBWA <- df_merged$Wicket_Taking_Ability_17

df_merged$Bowling_points_17 <- ((TBWA/PBWA) + (TBWER/PBWER)^2)*PWT

df_merged[is.na(df_merged$Bowling_points_17),][,'Bowling_points_17'] <- 0
df_merged[is.na(df_merged$Batting_points_17),][,'Batting_points_17'] <- 0


df_merged$MVPI_17 <- df_merged$Batting_points_17 + df_merged$Bowling_points_17


df_ipl_model <- df_merged[,c(colnames(df_merged)[1:7],colnames(df_merged)[18:22], colnames(df_merged)[100])]

#View(df_ipl)
#View(df_ipl_metric)
#View(df_ipl_model)
df_ipl_model[is.na(df_ipl_model)] <- 0

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df_ipl_model[,3:12], df_ipl_model[,13], sizes=10, rfeControl=control)


batting_results <- results$variables[c(91:95),]
batting_results$Overall <- batting_results$Overall/sum(batting_results$Overall)
batting_results

df_ipl_model$Batting_index_17 <- batting_results[1, 'Overall']*df_ipl_model$Consistency_bat_17 + batting_results[2, 'Overall']*df_ipl_model$hard_Hitting_Ability_17 +  batting_results[3, 'Overall']*df_ipl_model$Finisher_17 + batting_results[4, 'Overall']*df_ipl_model$Fast_Scoring_ability_17 + batting_results[5, 'Overall']*df_ipl_model$RunsBetweenWicket_17
bowling_results <- results$variables[c(96:100),]
bowling_results$Overall <- bowling_results$Overall/sum(bowling_results$Overall)
bowling_results

df_ipl_model$Bowling_index_17 <- bowling_results[1,'Overall']*df_ipl_model$Short_Performance_Index_17 + bowling_results[2,'Overall']*df_ipl_model$Economy_17 +  bowling_results[3,'Overall']*df_ipl_model$Ball_Consistency_17 + bowling_results[4,'Overall']*df_ipl_model$Wicket_Taking_Ability_17 + bowling_results[5,'Overall']*df_ipl_model$Crucial_Wicket_Taking_Ability_17

df_ipl_model$All_rounder_index_17 <- df_ipl_model$Batting_index_17/sum(df_ipl_model$Batting_index_17) + df_ipl_model$Bowling_index_17/sum(df_ipl_model$Bowling_index_17)

#----------------------------------17_16-----------------------------------------------

TBA <- sum(df_merged$Runs_17_16)/sum(df_merged$Wickets_taken_17_16)
TBSR <- sum(df_merged$Runs_17_16)/sum(df_merged$BallsFaced_17_16)
df_merged$Batting_points_17_16 <- (df_merged$Consistency_bat_17_16/TBA)*df_merged$Runs_17_16 + (df_merged$Fast_Scoring_ability_17_16/TBSR)*df_merged$Runs_17_16


TBB <- sum(df_merged$balls_bowled_17_16)
TWT <- sum(df_merged$Wickets_taken_17_16)
TBWA <- TBB/TWT
TRC <- sum(df_merged$runs_given_17_16)
TBWER <- (TRC*6)/TBB
PBWER <- (df_merged$runs_given_17_16*6)/df_merged$balls_bowled_17_16
PWT <- df_merged$Wickets_taken_17_16
PBWA <- df_merged$Wicket_Taking_Ability_17_16

df_merged$Bowling_points_17_16 <- ((TBWA/PBWA) + (TBWER/PBWER)^2)*PWT

df_merged[is.na(df_merged$Bowling_points_17_16),][,'Bowling_points_17_16'] <- 0
df_merged[is.na(df_merged$Batting_points_17_16),][,'Batting_points_17_16'] <- 0


df_merged$MVPI_17_16 <- df_merged$Batting_points_17_16 + df_merged$Bowling_points_17_16

df_ipl_model_17_16 <- df_merged[,c(colnames(df_merged)[1:2],colnames(df_merged)[8:12],colnames(df_merged)[23:27], colnames(df_merged)[103])]

#View(df_ipl)
#View(df_ipl_metric)
#View(df_ipl_model)
df_ipl_model_17_16[is.na(df_ipl_model_17_16)] <- 0


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df_ipl_model_17_16[,3:12], df_ipl_model_17_16[,13], sizes=10, rfeControl=control)
results$variables

batting_results <- results$variables[c(91:95),]
batting_results$Overall <- batting_results$Overall/sum(batting_results$Overall)
batting_results
df_ipl_model_17_16$Batting_index_17_16 <- batting_results[1, 'Overall']*df_ipl_model_17_16$Consistency_bat_17_16 + batting_results[2, 'Overall']*df_ipl_model_17_16$hard_Hitting_Ability_17_16 +  batting_results[3, 'Overall']*df_ipl_model_17_16$Finisher_17_16 + batting_results[4, 'Overall']*df_ipl_model_17_16$Fast_Scoring_ability_17_16 + batting_results[5, 'Overall']*df_ipl_model_17_16$RunsBetweenWicket_17_16


bowling_results <- results$variables[c(96:100),]
bowling_results$Overall <- bowling_results$Overall/sum(bowling_results$Overall)
bowling_results
df_ipl_model_17_16$Bowling_index_17_16 <- bowling_results[1,'Overall']*df_ipl_model_17_16$Short_Performance_Index_17_16 + bowling_results[2,'Overall']*df_ipl_model_17_16$Economy_17_16 +  bowling_results[3,'Overall']*df_ipl_model_17_16$Ball_Consistency_17_16 + bowling_results[4,'Overall']*df_ipl_model_17_16$Wicket_Taking_Ability_17_16 + bowling_results[5,'Overall']*df_ipl_model_17_16$Crucial_Wicket_Taking_Ability_17_16


df_ipl_model_17_16$All_rounder_index_17_16 <- df_ipl_model_17_16$Batting_index_17_16/sum(df_ipl_model_17_16$Batting_index_17_16) + df_ipl_model_17_16$Bowling_index_17_16/sum(df_ipl_model_17_16$Bowling_index_17_16)

#-------------------------------17_16_15--------------------------------------------------

TBA <- sum(df_merged$Runs_17_16_15)/sum(df_merged$Wickets_taken_17_16_15)
TBSR <- sum(df_merged$Runs_17_16_15)/sum(df_merged$BallsFaced_17_16_15)
df_merged$Batting_points_17_16_15 <- (df_merged$Consistency_bat_17_16_15/TBA)*df_merged$Runs_17_16_15 + (df_merged$Fast_Scoring_ability_17_16_15/TBSR)*df_merged$Runs_17_16_15


TBB <- sum(df_merged$balls_bowled_17_16_15)
TWT <- sum(df_merged$Wickets_taken_17_16_15)
TBWA <- TBB/TWT
TRC <- sum(df_merged$runs_given_17_16_15)
TBWER <- (TRC*6)/TBB
PBWER <- (df_merged$runs_given_17_16_15*6)/df_merged$balls_bowled_17_16_15
PWT <- df_merged$Wickets_taken_17_16_15
PBWA <- df_merged$Wicket_Taking_Ability_17_16_15

df_merged$Bowling_points_17_16_15 <- ((TBWA/PBWA) + (TBWER/PBWER)^2)*PWT

df_merged[is.na(df_merged$Bowling_points_17_16_15),][,'Bowling_points_17_16_15'] <- 0
df_merged[is.na(df_merged$Batting_points_17_16_15),][,'Batting_points_17_16_15'] <- 0


df_merged$MVPI_17_16_15 <- df_merged$Batting_points_17_16_15 + df_merged$Bowling_points_17_16_15

df_ipl_model_17_16_15 <- df_merged[,c(colnames(df_merged)[1:2],colnames(df_merged)[13:17],colnames(df_merged)[28:32], colnames(df_merged)[106])]

#View(df_ipl)
#View(df_ipl_metric)
#View(df_ipl_model_17_16_15)
df_ipl_model_17_16_15[is.na(df_ipl_model_17_16_15)] <- 0


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df_ipl_model_17_16_15[,3:12], df_ipl_model_17_16_15[,13], sizes=10, rfeControl=control)
results$variables

batting_results <- results$variables[c(91:95),]
batting_results$Overall <- batting_results$Overall/sum(batting_results$Overall)
batting_results
df_ipl_model_17_16_15$Batting_index_17_16_15 <- batting_results[1, 'Overall']*df_ipl_model_17_16_15$Consistency_bat_17_16_15 + batting_results[2, 'Overall']*df_ipl_model_17_16_15$hard_Hitting_Ability_17_16_15 +  batting_results[3, 'Overall']*df_ipl_model_17_16_15$Finisher_17_16_15 + batting_results[4, 'Overall']*df_ipl_model_17_16_15$Fast_Scoring_ability_17_16_15 + batting_results[5, 'Overall']*df_ipl_model_17_16_15$RunsBetweenWicket_17_16_15



bowling_results <- results$variables[c(96:100),]
bowling_results$Overall <- bowling_results$Overall/sum(bowling_results$Overall)
bowling_results
df_ipl_model_17_16_15$Bowling_index_17_16_15 <- bowling_results[1,'Overall']*df_ipl_model_17_16_15$Short_Performance_Index_17_16_15 + bowling_results[2,'Overall']*df_ipl_model_17_16_15$Economy_17_16_15 +  bowling_results[3,'Overall']*df_ipl_model_17_16_15$Ball_Consistency_17_16_15 + bowling_results[4,'Overall']*df_ipl_model_17_16_15$Wicket_Taking_Ability_17_16_15 + bowling_results[5,'Overall']*df_ipl_model_17_16_15$Crucial_Wicket_Taking_Ability_17_16_15


df_ipl_model_17_16_15$All_rounder_index_17_16_15 <- df_ipl_model_17_16_15$Batting_index_17_16_15/sum(df_ipl_model_17_16_15$Batting_index_17_16_15) + df_ipl_model_17_16_15$Bowling_index_17_16_15/sum(df_ipl_model_17_16_15$Bowling_index_17_16_15)

#---------------------------merging----------------------------

df_ipl_mvpi <- merge(x = df_ipl_model[,c(1,2,13:16)], y = df_ipl_model_17_16[,c(1,13:16)], by = c("Player_Id"), all.x = TRUE)

df_ipl_mvpi <- merge(x = df_ipl_mvpi, y = df_ipl_model_17_16_15[,c(1,13:16)], by = c("Player_Id"), all.x = TRUE)

df_ipl_mvpi$Batting_index_17_rank  <-  rank(-df_ipl_mvpi$Batting_index_17)
df_ipl_mvpi$Bowling_index_17_rank <- rank(-df_ipl_mvpi$Bowling_index_17)
df_ipl_mvpi$All_rounder_index_17_rank <- rank(-df_ipl_mvpi$All_rounder_index_17)

df_ipl_mvpi$Batting_index_17_16_rank  <-  rank(-df_ipl_mvpi$Batting_index_17_16)
df_ipl_mvpi$Bowling_index_17_16_rank <- rank(-df_ipl_mvpi$Bowling_index_17_16)
df_ipl_mvpi$All_rounder_index_17_16_rank <- rank(-df_ipl_mvpi$All_rounder_index_17_16)

df_ipl_mvpi$Batting_index_17_16_15_rank  <-  rank(-df_ipl_mvpi$Batting_index_17_16_15)
df_ipl_mvpi$Bowling_index_17_16_15_rank <- rank(-df_ipl_mvpi$Bowling_index_17_16_15)
df_ipl_mvpi$All_rounder_index_17_16_15_rank <- rank(-df_ipl_mvpi$All_rounder_index_17_16_15)


#df_ipl_metric gives final ranking of players by bowling batting and allround..
#ranked based on cumulative seasons - 2017, 2017 - 2016, 2017 - 2015

View(df_ipl_mvpi)
View(df_ipl_metric)
View(df_ipl)

