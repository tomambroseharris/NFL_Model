# Manipulating Rushing Data

Rushing_2016 <- Raw_Rushing_2016
Rushing_2017 <- Raw_Rushing_2017
Rushing_2018 <- Raw_Rushing_2018 
Rushing_2019 <- Raw_Rushing_2019 


# Create a rushing data vector for loops
Rushing_Data_Frames <- c("Rushing_2016", "Rushing_2017", "Rushing_2018", "Rushing_2019")



# ---- summarising yards for the team ----

for(i in Rushing_Data_Frames){
  
  Data <- get(i) %>%
    group_by(Date, Team, Game_number, Season) %>%
    summarise(Rushing_Yards = sum(Rushing_Yards),
              Attempted_Rushes = sum(Attempted_Rushes), .groups = "drop") 
  
  assign(i, Data)
  
}

rm(i, Data)

# ---- summarising season average ----

# Create new data frames that will eventually be joined onto the rushing data frames. 

for(i in Rushing_Data_Frames){
  
  Data <- get(i) 
  
  Data <- Data %>%
    group_by(Team) %>%
    summarise(!!as.name(paste0("Av_Rushing_Yards_", "For_Full_Current_Season")) := mean(Rushing_Yards),
              !!as.name(paste0("Av_Rushing_Attempts_", "For_Full_Current_Season")) := mean(Attempted_Rushes))

  
  assign(paste0("Full_Season_Average_",i), Data)
  
}

rm(i, Data)




# ---- Previous week's rushing yards ----



Rushing_2016 <- Generic_Lagging_Function(Rushing_2016, "Team","Rushing_Yards")
Rushing_2017 <- Generic_Lagging_Function(Rushing_2017, "Team","Rushing_Yards")
Rushing_2018 <- Generic_Lagging_Function(Rushing_2018, "Team","Rushing_Yards")
Rushing_2019 <- Generic_Lagging_Function(Rushing_2019, "Team","Rushing_Yards")





# Summarising rolling average




Rushing_2016 <- Season_Rolling_Average_RYds_Function(Rushing_2016, "Team", "Rushing_Yards")
Rushing_2017 <- Season_Rolling_Average_RYds_Function(Rushing_2017, "Team", "Rushing_Yards")
Rushing_2018 <- Season_Rolling_Average_RYds_Function(Rushing_2018, "Team", "Rushing_Yards")
Rushing_2019 <- Season_Rolling_Average_RYds_Function(Rushing_2019, "Team", "Rushing_Yards")

# Lagging the season average rushing yards, too
Rushing_2016 <- Generic_Lagging_Function(Rushing_2016, "Team","To_Date_This_Season_Average_Rushing_Yards")
Rushing_2017 <- Generic_Lagging_Function(Rushing_2017, "Team","To_Date_This_Season_Average_Rushing_Yards")
Rushing_2018 <- Generic_Lagging_Function(Rushing_2018, "Team","To_Date_This_Season_Average_Rushing_Yards")
Rushing_2019 <- Generic_Lagging_Function(Rushing_2019, "Team","To_Date_This_Season_Average_Rushing_Yards")



# Deselecting unecceessary columns


for(i in Rushing_Data_Frames){
  
  Data <- get(i) 
  
  Data <- Data %>%
    select(-c(Attempted_Rushes, Rushing_Yards))
  
  assign(i, Data)
  
}
  
  


