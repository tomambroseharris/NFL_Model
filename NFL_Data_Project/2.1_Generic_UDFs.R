# Build user-defined functions to manipulate the data

# ---- Lagging ----

# Adding a column for the previous week's passing yards
Generic_Lagging_Function <- function(Data_Frame,
                                     Unique_Entry_Column,
                                     Col_To_Lag){
  
  # Create a shell data frame with no data in it
  # Filtering for something that doesn't exist will remove all rows
  DF_Shell <- Data_Frame %>%
    filter(Game_number == "")
  
  
  # run a loop  around unique instances in the unique entry column
  for(i in unique(Data_Frame[[Unique_Entry_Column]])){
    
    # define a working data frame
    Working_Data <- Data_Frame %>%
      # filter the unique entry column to only equal i
      filter(!!as.name(Unique_Entry_Column) == i) %>%
      # arrange in order of game number
      arrange(Game_number) %>%
      # create a new lagged column - the default for the lagging function is to only lag by 1
      mutate(!!as.name(paste0("Previous_Week_", Col_To_Lag)) := lag(!!as.name(Col_To_Lag)))
    
    
    # Bind the daata frame we've created onto the existing data frame
    DF_Shell <- DF_Shell %>%
      bind_rows(Working_Data)
    
  }
  
  # return the bound data frame
  return(DF_Shell)
  
}


# ---- Define a function that creates a rolling average function for the season ----



Season_Rolling_Average_RYds_Function <- function(Data_Frame,
                                                 Unique_Entry_Column,
                                                 Col_To_Roll_Average_Of){
  
  # create a shell data frame
  DF_Shell <- Data_Frame %>%
    filter(Game_number == "")
  
  # For each unique value in the unique entry column, run a loop
  for(i in unique(Data_Frame[[Unique_Entry_Column]])){
    
    # run a function for each player in the data frame
    Working_Data <- Data_Frame %>%
      filter(!!as.name(Unique_Entry_Column) == i) %>%
      arrange(Game_number)
    
    # for each number of rows in the data
    for(j in 1:nrow(Working_Data)){
      
      # slice the top j rows, create the average of those, then do the same for the next bit of data
      Working_Average_Data_Frame <- Working_Data %>%
        slice_head(n = j) %>%
        mutate(!!as.name(paste0("To_Date_This_Season_Average_", Col_To_Roll_Average_Of)) := mean(!!as.name(Col_To_Roll_Average_Of), na.rm = T)) %>%
        # only take the last row
        slice_tail(n = 1)
      
      # bind the data back together
      DF_Shell <- DF_Shell %>%
        bind_rows(Working_Average_Data_Frame)
      
    }
    
  }
  
  # return the bound data frame
  return(DF_Shell)
  
}




