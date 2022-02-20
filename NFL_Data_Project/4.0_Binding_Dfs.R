# Use this script to bind data frames together 

#1. Passing data - joining the previuos seasons

Passing_2019 <- Passing_2019 %>% left_join(Full_Season_Average_Passing_2018, by = "Player") %>%
  left_join(Full_Season_Average_Rushing_2018, by = "Team") %>%
  left_join(Full_Season_Average_D_Pass_2018, by = "Opponent") %>%
  left_join(Full_Season_Average_D_Rush_2018, by = "Opponent") %>%
  left_join(D_Pass_2019, by = c("Opponent", "Game_number", "Season")) %>%
  left_join(Rushing_2019, by = c("Date", "Team", "Game_number", "Season")) 

Passing_2018 <- Passing_2018 %>% left_join(Full_Season_Average_Passing_2017, by = "Player") %>%
  left_join(Full_Season_Average_Rushing_2017, by = "Team") %>%
  left_join(Full_Season_Average_D_Pass_2017, by = "Opponent") %>%
  left_join(Full_Season_Average_D_Rush_2017, by = "Opponent") %>%
  left_join(D_Pass_2018, by = c("Opponent", "Game_number", "Season")) %>%
  left_join(Rushing_2018, by = c("Date", "Team", "Game_number", "Season")) 

Passing_2017 <- Passing_2017 %>% left_join(Full_Season_Average_Passing_2016, by = "Player") %>%
  left_join(Full_Season_Average_Rushing_2016, by = "Team") %>%
  left_join(Full_Season_Average_D_Pass_2016, by = "Opponent") %>%
  left_join(Full_Season_Average_D_Rush_2016, by = "Opponent") %>%
  left_join(D_Pass_2017, by = c("Opponent", "Game_number", "Season")) %>%
  left_join(Rushing_2017, by = c("Date", "Team", "Game_number", "Season")) 
                       




  
##########

# vertically bind the passing dfs
Core_Passing <- Passing_2019 %>%
  bind_rows(Passing_2018, Passing_2017) %>%
  left_join(Previous_Player_Opponent, by = c("Player", "Date", "Opponent")) %>%
  mutate(home_team = if_else(Venue == "@", Opponent, Team),
         away_team = if_else(Venue == "@", Team, Opponent))


# select key columns from game ID data.
Clean_Game_IDs <- Game_ID_Data %>%
  mutate(season = as.character(season)) %>%
  mutate(season = case_when(season == "2016" ~ "2016/17",
                            season == "2017" ~ "2017/18",
                            season == "2018" ~ "2018/19",
                            season == "2019" ~ "2019/20",
                            TRUE ~ season)) %>%
  select(-c(gameday, stadium))








