# Create manipulatable data frames
Passing_2016 <- Raw_Passing_2016
Passing_2017 <- Raw_Passing_2017
Passing_2018 <- Raw_Passing_2018
Passing_2019 <- Raw_Passing_2019

# create passing vector data
Passing_Data_Frames <- c("Passing_2016", "Passing_2017", "Passing_2018", "Passing_2019")


# Creating a betting line df with only averages in 

Player_Av_Passing_Yards <- Passing_2019 %>%
  bind_rows(Passing_2016, Passing_2017, Passing_2018) %>%
  group_by(Player) %>%
  summarise(Av_Passing = round(mean(Passing_Yards), 1),
            games_played = n())
            
            

# Lagging columns



Passing_2016 <- Generic_Lagging_Function(Passing_2016, "Player","Passing_Yards")
Passing_2017 <- Generic_Lagging_Function(Passing_2017, "Player","Passing_Yards")
Passing_2018 <- Generic_Lagging_Function(Passing_2018, "Player","Passing_Yards")
Passing_2019 <- Generic_Lagging_Function(Passing_2019, "Player","Passing_Yards")



#####################################################################################


# Add column for rolling average of the season. 




Passing_2016 <- Season_Rolling_Average_RYds_Function(Passing_2016, "Player", "Passing_Yards")
Passing_2017 <- Season_Rolling_Average_RYds_Function(Passing_2017, "Player", "Passing_Yards")
Passing_2018 <- Season_Rolling_Average_RYds_Function(Passing_2018, "Player", "Passing_Yards")
Passing_2019 <- Season_Rolling_Average_RYds_Function(Passing_2019, "Player", "Passing_Yards")

# lagging the season average passing yards, too.
Passing_2016 <- Generic_Lagging_Function(Passing_2016, "Player","To_Date_This_Season_Average_Passing_Yards")
Passing_2017 <- Generic_Lagging_Function(Passing_2017, "Player","To_Date_This_Season_Average_Passing_Yards")
Passing_2018 <- Generic_Lagging_Function(Passing_2018, "Player","To_Date_This_Season_Average_Passing_Yards")
Passing_2019 <- Generic_Lagging_Function(Passing_2019, "Player","To_Date_This_Season_Average_Passing_Yards")




# Summarise average for each year


for(i in Passing_Data_Frames){
  
  Data <- get(i) 
  
  
  Data <- Data %>%
    group_by(Player) %>%
    summarise(!!as.name(paste0("Av_Passing_Yards_", "For_Full_Previous_Season")) := mean(Passing_Yards),
              !!as.name(paste0("Av_Passing_Attempts_", "For_Full_Previous_Season")) := mean(Attempted_Passes),
              Av_Comp_Percent_Prev_Season := (sum(Completed_Passes)/sum(Attempted_Passes)))
  
  # Rushing_Df <- get(i) %>%
  #   left_join(Data, by = "Team")
  
  assign(paste0("Full_Season_Average_",i), Data)
  
}

# remove the redundant objects
rm(i, Data)










