
# renaming all the teams so I can join the data frames using identical name formats
# different names exist in the GameID and passing/rushing data

Team_Name_Function <- function(Input_Data, Columns_Vector){
  
  for(i in Columns_Vector){
    
    
    Input_Data <- Input_Data %>%
      mutate(!!as.name(i) := case_when(!!as.name(i) %in% "ARI" ~ "Cardinals",
                                       !!as.name(i) %in% "ATL" ~ "Falcons",
                                       !!as.name(i) %in% "BAL" ~ "Ravens",
                                       !!as.name(i) %in% "BUF" ~ "Bills",
                                       !!as.name(i) %in% "CAR" ~ "Panthers",
                                       !!as.name(i) %in% "CHI" ~ "Bears",
                                       !!as.name(i) %in% "CIN" ~ "Bengals",
                                       !!as.name(i) %in% "CLE" ~ "Browns",
                                       !!as.name(i) %in% "DAL" ~ "Cowboys",
                                       !!as.name(i) %in% "DEN" ~ "Broncos",
                                       !!as.name(i) %in% "DET" ~ "Lions",
                                       !!as.name(i) %in% c("GB", "GNB") ~ "Packers",
                                       !!as.name(i) %in% "HOU" ~ "Texans",
                                       !!as.name(i) %in% "IND" ~ "Colts",
                                       !!as.name(i) %in% "JAX" ~ "Jaguars",
                                       !!as.name(i) %in% c("KC", "KAN") ~ "Chiefs",
                                       !!as.name(i) %in% c("LAC", "SDG", "SD") ~ "Chargers",
                                       !!as.name(i) %in% c("LAR", "LA") ~ "Rams",
                                       !!as.name(i) %in% "MIA" ~ "Dolphins",
                                       !!as.name(i) %in% "MIN" ~ "Vikings",
                                       !!as.name(i) %in% c("NWE", "NE")  ~ "Patriots",
                                       !!as.name(i) %in% c("NO", "NOR") ~ "Saints",
                                       !!as.name(i) %in% "NYG" ~ "Giants",
                                       !!as.name(i) %in% "NYJ" ~ "Jets",
                                       !!as.name(i) %in% "OAK" ~ "Raiders",
                                       !!as.name(i) %in% "PHI" ~ "Eagles",
                                       !!as.name(i) %in% "PIT" ~ "Steelers",
                                       !!as.name(i) %in% c("SF", "SFO") ~ "49ers",
                                       !!as.name(i) %in% c("TB", "TAM") ~ "Buccaneers",
                                       !!as.name(i) %in% "SEA" ~ "Seahawks",
                                       !!as.name(i) %in% "TEN" ~ "Titans",
                                       !!as.name(i) %in% "WAS" ~ "Washington",
                           TRUE ~ "STOP - ERROR"))
  }
  
  return(Input_Data)
}


# Run the function over the Game_ID frame
Game_ID_Data <- Team_Name_Function(Game_ID_Data, c("home_team", "away_team"))

# Run the function over the passing and rushing frames
Raw_Passing_2016 <- Team_Name_Function(Raw_Passing_2016, c("Team", "Opponent"))
Raw_Passing_2017 <- Team_Name_Function(Raw_Passing_2017, c("Team", "Opponent"))
Raw_Passing_2018 <- Team_Name_Function(Raw_Passing_2018, c("Team", "Opponent"))
Raw_Passing_2019 <- Team_Name_Function(Raw_Passing_2019, c("Team", "Opponent"))

Raw_Rushing_2016 <- Team_Name_Function(Raw_Rushing_2016, c("Team", "Opponent"))
Raw_Rushing_2017 <- Team_Name_Function(Raw_Rushing_2017, c("Team", "Opponent"))
Raw_Rushing_2018 <- Team_Name_Function(Raw_Rushing_2018, c("Team", "Opponent"))
Raw_Rushing_2019 <- Team_Name_Function(Raw_Rushing_2019, c("Team", "Opponent"))



