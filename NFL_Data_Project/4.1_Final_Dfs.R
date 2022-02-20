# create the core data frame

Core_Data_Frame <- Core_Passing %>%
  # join game IDs & weather data
  inner_join(Clean_Game_IDs, by = c("Season" = "season", "home_team" = "home_team", "away_team" = "away_team")) %>%
  left_join(Weather_Data, by = c("old_game_id" = "game_id")) %>%
  # deselect unnecessary columns
  select(-c(Position, Day, TD, Result, Incomplete_Passes, Completed_Passes, -Attempted_Passes,
            home_team, away_team, old_game_id, temp, wind, surface)) %>%
  # remove weather data if stadium is indoors
  mutate(Precipitation = if_else(roof %in% c("closed", "dome"), 0, Precipitation)) %>%
  mutate(WindSpeed = if_else(roof %in% c("closed", "dome"), 0, WindSpeed)) %>%
  mutate(Temperature = if_else(roof %in% c("closed", "dome"), 70, Temperature)) %>%
 # round the numeric columns
  mutate(across(c(Passing_Yards,
                  Previous_Week_Passing_Yards,
                  Previous_Week_Rushing_Yards,
                  Previous_Week_Opponent_Conceded_Passing_Yards,
                  To_Date_This_Season_Average_Passing_Yards,
                  To_Date_This_Season_Average_Rushing_Yards,
                  To_Date_This_Season_Average_Opponent_Conceded_Passing_Yards,
                  To_Date_This_Season_Average_Opponent_Conceded_Attempted_Passes,
                  Av_Passing_Yards_For_Full_Previous_Season,
                  Av_Passing_Attempts_For_Full_Previous_Season,
                  Av_Rushing_Yards_For_Full_Current_Season,
                  Av_Rushing_Attempts_For_Full_Current_Season,
                  Av_Conceded_Passing_Yards_For_Full_Previous_Season,
                  Av_Conceded_Passing_Attempts_For_Full_Previous_Season,
                  Av_Conceded_Rushing_Yards_For_Full_Previous_Season,
                  Av_Conceded_Rushing_Atts_For_Full_Previous_Season,
                  Passing_Yards_In_Last_Game_Vs_Opponent,
                  Att_Passes_In_Last_Game_Vs_Opponent,
                  Precipitation,
                  WindSpeed,
                  Temperature), ~ round(., 0))) %>%
  # round completion percentage to 2 decimal places
  mutate(Av_Comp_Percent_Prev_Season = round(Av_Comp_Percent_Prev_Season, 2)) %>%
  mutate(Outdoors = if_else(roof %in% c("open", "outdoors"), 1, 0)) %>%
  select(-roof) %>%
  rename("P_Yds" = "Passing_Yards",
         "Prev_Wk_P_Yds" = "Previous_Week_Passing_Yards",
         "Seas_Av_P_Yds" = "To_Date_This_Season_Average_Passing_Yards",
         "Prev_Seas_Av_P_Yds" = "Av_Passing_Yards_For_Full_Previous_Season",
         "Prev_Seas_Av_P_Atts" = "Av_Passing_Attempts_For_Full_Previous_Season",
         "Prev_Seas_Comp_Perc" = "Av_Comp_Percent_Prev_Season",
         "Opp_Prev_Seas_Conc_P_Yds" = "Av_Conceded_Passing_Yards_For_Full_Previous_Season",            
         "Opp_Prev_Seas_Conc_P_Atts" = "Av_Conceded_Passing_Attempts_For_Full_Previous_Season",         
         "Opp_Prev_Seas_Conc_R_Yds" = "Av_Conceded_Rushing_Yards_For_Full_Previous_Season",            
         "Opp_Prev_Seas_Conc_R_Atts" = "Av_Conceded_Rushing_Atts_For_Full_Previous_Season" ,            
         "Opp_Seas_Av_P_Yds" = "To_Date_This_Season_Average_Opponent_Conceded_Passing_Yards",
         "Opp_Seas_Av_P_Atts" = "To_Date_This_Season_Average_Opponent_Conceded_Attempted_Passes",
         "Opp_Prev_Wk_P_Yds" = "Previous_Week_Opponent_Conceded_Passing_Yards",
         "Prev_Wk_R_Yds" = "Previous_Week_Rushing_Yards",                                   
         "Seas_Av_R_Yds" = "To_Date_This_Season_Average_Rushing_Yards",                     
         "Prev_Vs_Opp_P_Yds" = "Passing_Yards_In_Last_Game_Vs_Opponent",                        
         "Prev_Vs_Opp_P_Atts" = "Att_Passes_In_Last_Game_Vs_Opponent"  ) %>%
  mutate(Venue = if_else(Venue == "@", 1, 0)) %>%
  rename("Home" = "Venue") %>%
  select(-c(Av_Rushing_Attempts_For_Full_Current_Season,
            Av_Rushing_Yards_For_Full_Current_Season,
            Previous_Week_To_Date_This_Season_Average_Passing_Yards))

Core_Data_Frame <- Core_Data_Frame %>%
  group_by(Player, Season) %>%
  arrange(Game_number) %>%
  mutate(Seas_Av_P_Yds = lag(Seas_Av_P_Yds, n = 1),
         Seas_Av_R_Yds = lag(Seas_Av_R_Yds, n = 1))

Core_Data_Frame_Past_Wk2 <- Core_Data_Frame %>%
  filter(Game_number > 2) 



Core_Data_Frame_Past_Wk2 <- Core_Data_Frame_Past_Wk2 %>%
  left_join(Player_Av_Passing_Yards, by = "Player") %>%
  mutate(Over_B_Line = if_else(P_Yds > (Av_Passing*(1+(sample(5:15, 1)/100))), 1, 0)) %>%
  mutate(Under_B_Line = if_else(P_Yds < (Av_Passing*(1-(sample(5:15, 1)/100))), 1, 0)) %>%
  mutate(mean_line = if_else(P_Yds > (Av_Passing), 1, 0)) %>%
  ungroup() %>%
  select(-c(Av_Passing, games_played)) %>%
  filter(Prev_Wk_P_Yds != Seas_Av_P_Yds)


write.csv(Core_Data_Frame_Past_Wk2, file = "NFL_Past_Wk2.csv", row.names = FALSE)


Multi_Season_DF_Past_Wk2 <- Core_Data_Frame_Past_Wk2 %>%
  left_join(Player_Av_Passing_Yards, by = "Player") %>%
  filter(Prev_Wk_P_Yds != Seas_Av_P_Yds,
         games_played > 16) %>%
  select(-c(Av_Passing, games_played, Previous_Week_To_Date_This_Season_Average_Rushing_Yards, Age))

names(Multi_Season_DF_Past_Wk2)


write.csv(Multi_Season_DF_Past_Wk2, file = "Multi_Season_DF_Past_Wk2.csv", row.names = FALSE)


# rm(All_QB_Opponent_Data)
# rm(D_Pass_Dfs, D_Pass_2016, D_Rush_2016, D_Rush_2017, D_Pass_2017,
#    D_Rush_Dfs, D_Pass_2018, D_Rush_2018, D_Pass_2019, D_Rush_2019)
# 
# rm(Full_Season_Average_D_Pass_2016, Full_Season_Average_D_Pass_2017, Full_Season_Average_D_Pass_2018,
#    Full_Season_Average_D_Rush_2016, Full_Season_Average_D_Pass_2019, Full_Season_Average_D_Rush_2017,
#    Full_Season_Average_D_Rush_2018, Full_Season_Average_D_Rush_2019, Full_Season_Average_Passing_2016,
#    Full_Season_Average_Passing_2017, Full_Season_Average_Passing_2018, Full_Season_Average_Passing_2019,
#    Full_Season_Average_Rushing_2016, Full_Season_Average_Rushing_2018, Full_Season_Average_Rushing_2017,
#    Full_Season_Average_Rushing_2019)
# 
# rm(Passing_2016, Passing_2017,  Passing_2018, Rushing_2016, Rushing_2017,
#    Rushing_2018, Rushing_2019, Rushing_Data_Frames, Previous_Player_Opponent, Passing_Data_Frames,
#    Player_Opponent_Function, Season_Rolling_Average_RYds_Function)
# 
# rm(Raw_Passing_2017, Raw_Passing_2019, Raw_Passing_2016, Raw_Passing_2018, Raw_Passing_Data,
#    Raw_Rushing_2017, Raw_Rushing_2016, Raw_Rushing_2018, Raw_Rushing_Data, Raw_Rushing_2019,
#    Team_Name_Function, Generic_Lagging_Function, data)
# 
