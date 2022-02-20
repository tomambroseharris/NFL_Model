
# Load the raw passing data
Raw_Passing_2016 <- read.csv("Data_Files/2016 passing yards w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2016/17")
Raw_Passing_2017 <- read.csv("Data_Files/2017 passing yards w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2017/18")
Raw_Passing_2018 <- read.csv("Data_Files/2018 passing yards w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2018/19")
Raw_Passing_2019 <- read.csv("Data_Files/2019 passing yards w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2019/20")

# load the raw rushing data
Raw_Rushing_2016 <- read.csv("Data_Files/Player Rushing Yards 2016 w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2016/17")
Raw_Rushing_2017 <- read.csv("Data_Files/Player Rushing Yards 2017 w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2017/18")
Raw_Rushing_2018 <- read.csv("Data_Files/Player Rushing Yards 2018 w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2018/19")
Raw_Rushing_2019 <- read.csv("Data_Files/Player Rushing Yards 2019 w1 to 16.csv", fileEncoding = "UTF-8-BOM") %>%  mutate(Season = "2019/20")



# Vector of passing data frames for each week

Raw_Passing_Data <- c("Raw_Passing_2016", "Raw_Passing_2017", "Raw_Passing_2018", "Raw_Passing_2019")

# Select the relevant columns in each passing data frame
for(i in Raw_Passing_Data){
  
  # Assign an object, i within the vector, to `Data`
  Data <- get(i) %>%
    select(Player, Date, Position = Pos, Age, Team = Tm, Venue, Opponent = Opp, Result, Game_number, Day, Completed_Passes, Attempted_Passes, Incomplete_Passes, Passing_Yards,
           TD, Season) %>%
    # Make the columns numeric
    mutate(across(c(Game_number, Completed_Passes, Attempted_Passes, Incomplete_Passes, Passing_Yards, TD), as.numeric)) %>%
    #  Ensure the player was a quarterback
    filter(Position == "QB",
           Game_number %in% 1:16)
  
  
  assign(i, Data)
  
}


# create a vector of raw rushing data
Raw_Rushing_Data <- c("Raw_Rushing_2016","Raw_Rushing_2017","Raw_Rushing_2018","Raw_Rushing_2019")

# select the relevant column in each rushing data frame
for(i in Raw_Rushing_Data){
  
  # Assign a variable `Data` to the named variable in position i of the vector
  Data <- get(i) %>%
    select(Player, Date, Age, Team = Tm, Venue, Opponent = Opp, Result, Game_number = G., Day, Attempted_Rushes = Att, Rushing_Yards = Yds,
           TD, Season) %>%
    mutate(across(c(Game_number, Attempted_Rushes, Rushing_Yards), as.numeric)) %>%
    filter(Game_number %in% 1:16)
  
  
  assign(i, Data)
  
}

# Remove the objects created in the loops above
rm(i, Data)




############### Weather data #####################

# Define a function that does nothing
# The sole role of this is to document where the data have come from
# there is also the code to write the weather data .csv
function(.){

Weather_Data <- read.csv("https://raw.githubusercontent.com/ThompsonJamesBliss/WeatherData/master/data/games_weather.csv")
write.csv(Weather_Data, "Data_Files/Weather_Data", row.names = FALSE)
}

# read in the weather data
Weather_Data <- read.csv("Data_Files/Weather_Data") %>%
  # summarise the weather data for each game, as it is currently broken down by hours within the game
  group_by(game_id) %>%
  summarise(Precipitation = sum(Precipitation),
            WindSpeed = mean(WindSpeed),
            Temperature = mean(Temperature))


# ---- GAMEID Data -----

# As above, put data documentation and writing code into a function
function(.){
  
Game_ID_Data <- read.csv("http://www.habitatring.com/games.csv") %>%
  select(season, gameday, old_game_id, roof, surface, temp,  wind, stadium, home_team, away_team) 

write.csv(Game_ID_Data, "Data_Files/Game_ID_Data", row.names = FALSE)

}


# Read in the game id data and select the relevant columns
Game_ID_Data <- read.csv("http://www.habitatring.com/games.csv") %>%
  select(season, gameday, old_game_id, roof, surface, temp,  wind, stadium, home_team, away_team)
