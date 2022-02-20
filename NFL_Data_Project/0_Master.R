# Master script
# This script runs all the other scripts in the project


# Load packages script
source("1.0_Load_Packages.R")

# Load the raw data
source("2.0_Load_Data.R")

# Run the code to build user-defined functions. These manipulate the data
source("2.1_Generic_UDFs.R")

# A script to clean the team names i.e. to put them all into the same form
source("2.2_Clean_Team_Names.R")

#################################################################################

# Manipulating the passing data
source("3.0_Player_Passing_Data.R")
# manipulating the rushing data
source("3.1_Rushing_Data.R")
# manipulating the defensive data
source("3.2_Defensive_Data.R")
# manipulating the player opponent data
source("3.3_Player_Opponent_Data.R")

# bind the data frames together
source("4.0_Binding_Dfs.R")

# produce the final data frame.
source("4.1_Final_Dfs.R")


# write.csv(head(Raw_Passing_2017, 5), "Data frames/Passing.csv")
# write.csv(head(Raw_Rushing_2017, 5), "Data frames/Rushing.csv")
# write.csv(head(Weather_Data, 5), "Data frames/Weather.csv")
# write.csv(head(Clean_Game_IDs, 5), "Data frames/Game_IDs.csv")
# write.csv(as.data.frame(names(Core_Data_Frame_Past_Wk2)), "Data frames/Data columns.csv")

