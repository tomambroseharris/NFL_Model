
# Create defensive dfs
# these need to be split into rushing and passing

D_Pass_2016 <- Raw_Passing_2016 
D_Pass_2017 <- Raw_Passing_2017
D_Pass_2018 <- Raw_Passing_2018 
D_Pass_2019 <- Raw_Passing_2019 

# Assign vector of defensive passing data frames
D_Pass_Dfs <- c("D_Pass_2016", "D_Pass_2017", "D_Pass_2018", "D_Pass_2019")


# run a loop around defensive passing yards - 
for(i in D_Pass_Dfs){
  
  Data <- get(i) %>%
    group_by(Date, Opponent, Game_number, Season) %>%
   
    # this will summmarise for the entire game, how many the team conceded
    summarise(Opponent_Conceded_Passing_Yards = sum(Passing_Yards),
              Opponent_Conceded_Attempted_Passes = sum(Attempted_Passes), .groups = "drop")


  assign(i, Data)
  
}

rm(i, Data)


### Summarise averages ####


for(i in D_Pass_Dfs){
  
  Data <- get(i) 
  
  
  Data <- Data %>%
    group_by(Opponent) %>%
    summarise(!!as.name(paste0("Av_Conceded_Passing_Yards_", "For_Full_Previous_Season")) := mean(Opponent_Conceded_Passing_Yards),
              !!as.name(paste0("Av_Conceded_Passing_Attempts_", "For_Full_Previous_Season")) := mean(Opponent_Conceded_Attempted_Passes))

  
  assign(paste0("Full_Season_Average_",i), Data)
  
}

rm(i, Data)




# create season averages 
D_Pass_2016 <- Season_Rolling_Average_RYds_Function(D_Pass_2016, "Opponent", "Opponent_Conceded_Passing_Yards")
D_Pass_2017 <- Season_Rolling_Average_RYds_Function(D_Pass_2017, "Opponent", "Opponent_Conceded_Passing_Yards") 
D_Pass_2018 <- Season_Rolling_Average_RYds_Function(D_Pass_2018, "Opponent", "Opponent_Conceded_Passing_Yards")  
D_Pass_2019 <- Season_Rolling_Average_RYds_Function(D_Pass_2019, "Opponent", "Opponent_Conceded_Passing_Yards")  

D_Pass_2016 <- Season_Rolling_Average_RYds_Function(D_Pass_2016, "Opponent", "Opponent_Conceded_Attempted_Passes")
D_Pass_2017 <- Season_Rolling_Average_RYds_Function(D_Pass_2017, "Opponent", "Opponent_Conceded_Attempted_Passes") 
D_Pass_2018 <- Season_Rolling_Average_RYds_Function(D_Pass_2018, "Opponent", "Opponent_Conceded_Attempted_Passes")  
D_Pass_2019 <- Season_Rolling_Average_RYds_Function(D_Pass_2019, "Opponent", "Opponent_Conceded_Attempted_Passes")  

# lag conceded passes. 
D_Pass_2016 <- Generic_Lagging_Function(D_Pass_2016, "Opponent","Opponent_Conceded_Passing_Yards")
D_Pass_2017 <- Generic_Lagging_Function(D_Pass_2017, "Opponent","Opponent_Conceded_Passing_Yards")
D_Pass_2018 <- Generic_Lagging_Function(D_Pass_2018, "Opponent","Opponent_Conceded_Passing_Yards")
D_Pass_2019 <- Generic_Lagging_Function(D_Pass_2019, "Opponent","Opponent_Conceded_Passing_Yards")

# select key columns
for(i in D_Pass_Dfs){
  
  data <- get(i) %>%
    select(Opponent, Game_number, Season, 
           To_Date_This_Season_Average_Opponent_Conceded_Passing_Yards,
           To_Date_This_Season_Average_Opponent_Conceded_Attempted_Passes,
           Previous_Week_Opponent_Conceded_Passing_Yards)
  
  assign(i, data)
  
}

# Section 2 -----
########### RUSHING data frames ##################

D_Rush_2016 <- Raw_Rushing_2016
D_Rush_2017 <- Raw_Rushing_2017
D_Rush_2018 <- Raw_Rushing_2018
D_Rush_2019 <- Raw_Rushing_2019

D_Rush_Dfs <- c("D_Rush_2016", "D_Rush_2017", "D_Rush_2018", "D_Rush_2019")

for(i in D_Rush_Dfs){
  
  Data <- get(i) %>%
    group_by(Date, Opponent, Game_number, Season) %>%
    
    summarise(Conceded_Rushing_Yards = sum(Rushing_Yards),
              Conceded_Rushing_Atts = sum(Attempted_Rushes), .groups = "drop")
  
  
  assign(i, Data)
  
}

# Season averages

for(i in D_Rush_Dfs){
  
  Data <- get(i) 
  
  
  Data <- Data %>%
    group_by(Opponent) %>%
    summarise(!!as.name(paste0("Av_Conceded_Rushing_Yards_", "For_Full_Previous_Season")) := mean(Conceded_Rushing_Yards),
              !!as.name(paste0("Av_Conceded_Rushing_Atts_", "For_Full_Previous_Season")) := mean(Conceded_Rushing_Atts))
  
  # Rushing_Df <- get(i) %>%
  #   left_join(Data, by = "Team")
  
  assign(paste0("Full_Season_Average_",i), Data)
  
}

rm(i, Data)



################

# create averages. Lags not applied as the rushing yards aren't in a specific pre-week column in final data frame.
D_Rush_2016 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Yards")
D_Rush_2017 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Yards") 
D_Rush_2018 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Yards")  
D_Rush_2019 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Yards")  

D_Rush_2016 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Atts")
D_Rush_2017 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Atts") 
D_Rush_2018 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Atts")  
D_Rush_2019 <- Season_Rolling_Average_RYds_Function(D_Rush_2016, "Opponent", "Conceded_Rushing_Atts")  
