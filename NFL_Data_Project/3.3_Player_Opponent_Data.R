
# Create a data frame across all four seasons in case player played against team in a past season
All_QB_Opponent_Data <- Raw_Passing_2016 %>%
  bind_rows(Raw_Passing_2017, Raw_Passing_2018, Raw_Passing_2019) %>%
  select(Player, Date, Opponent, Passing_Yards, Attempted_Passes)



# define a function to identify player opponent
Player_Opponent_Function <- function(Input_Data){
  
  # create data frame shell to bind to
  Player_Opponent_Data_Shell <- data.frame(Player = c(),
                                       Date = c(), 
                                       Opponent = c(), 
                                       Passing_Yards_In_Last_Game_Vs_Opponent = c(), 
                                       Att_Passes_In_Last_Game_Vs_Opponent = c())
  

  # for each unique input in the player columm
for(i in unique(Input_Data$Player)){
  
  Player_Data <- Input_Data %>%
    filter(Player == i) %>%
    # separate out the date for ordering purposes.
    mutate(Day = str_sub(Date, 1, 2),
           Month = month(Date),
           Year = str_sub(Date, -4, -1)) %>%
    # order the data, so it can be lagged by one instance
    arrange(Year, Month, Day) %>%
    # group by opponent
    group_by(Opponent) %>%
    # lag the data after
    mutate(Passing_Yards_In_Last_Game_Vs_Opponent = lag(Passing_Yards),
           Att_Passes_In_Last_Game_Vs_Opponent = lag(Attempted_Passes)) %>%
    # select only key columns (i.e. de-select date columns)
    select(Player, Date, Opponent, Passing_Yards_In_Last_Game_Vs_Opponent, Att_Passes_In_Last_Game_Vs_Opponent)
  
  # bind the data frame for player i to the data frame shell
  Player_Opponent_Data_Shell <- Player_Opponent_Data_Shell %>%
    bind_rows(Player_Data)
  
}

return(Player_Opponent_Data_Shell)
}

# ~ 1200 NAs
# run the function
Previous_Player_Opponent <- Player_Opponent_Function(All_QB_Opponent_Data) 




