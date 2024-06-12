# Shooting sample reliability

# Load libraries
library(tidyverse)
library(Metrics)

# R-Squared Function
rsq <- function (x, y) cor(x, y) ^ 2

# Load data
shots <- read_csv("NBA_2024_Shots.csv")

# Drop unnecessary columns
shots_cleaned <- shots %>%
  select(-SEASON_1, -SEASON_2, -TEAM_ID, -PLAYER_ID, -GAME_ID, -HOME_TEAM, 
         -AWAY_TEAM, -EVENT_TYPE, -ZONE_NAME, -ZONE_ABB, -LOC_X, -LOC_Y)




########## Function Testing Parameters ########## 
data <- shots_cleaned
shot_min <- 1000
n_samples <- 100

results <- random_shot_samples(data, shot_min, n_samples)




########## Build Simulation Function ########## 
random_shot_samples <- function(data, shot_min, n_samples) { 
  # Get list of players with at least n shot attempts
  qualifying_players <- data %>%
    group_by(PLAYER_NAME) %>% 
    mutate(shot_num = row_number()) %>%
    filter(shot_num >= shot_min) %>%
    select(PLAYER_NAME) %>%
    unique()
  
  # Filter data on qualifying players
  data <- data %>% filter(PLAYER_NAME %in% qualifying_players$PLAYER_NAME)
  
  # Get qualified players' season-long %s
  season_long_pct <- data %>%
    group_by(PLAYER_NAME) %>% 
    summarize(season_pct = sum(SHOT_MADE)/n())
  
  # Run n random samples for correlation smoothing
  all_samples_metrics <- data.frame()
  for(i in seq(from=1, to=100, by=1)){
    # Create n-shot random samples for each player, calculate %s
    random_n_shots <- data.frame()
    for(i in seq(from=10, to=shot_min, by=10)){
      random_shots <- data %>% 
        group_by(PLAYER_NAME) %>% 
        slice_sample(n=i) %>% 
        summarize(n_shots = i,
                  sample_pct = sum(SHOT_MADE)/i)
      
      random_n_shots <- rbind(random_n_shots, random_shots)
    }
    
    # Join season-long %s onto random samples
    random_n_shots <- random_n_shots %>%
      inner_join(season_long_pct, by = c(PLAYER_NAME = "PLAYER_NAME"))
  
    # Get accuracy metrics by n-shots
    n_shots_accuracy <- data.frame()
    for(i in seq(from=10, to=shot_min, by=10)){
      subset_for_metrics <- random_n_shots %>% 
        filter(n_shots == i)
      
      r_squared = rsq(subset_for_metrics$sample_pct, subset_for_metrics$season_pct)
      
      rmse = rmse(subset_for_metrics$sample_pct, subset_for_metrics$season_pct)
      
      diff = abs(subset_for_metrics$sample_pct - subset_for_metrics$season_pct)
      
      app_df = data.frame(i, r_squared, rmse, diff)
      
      n_shots_accuracy <- rbind(n_shots_accuracy, app_df)
    }
    
    all_samples_metrics <- rbind(all_samples_metrics, n_shots_accuracy)
  }
  
  # Average accuracy metrics across all random samples
  n_shot_samples <- all_samples_metrics %>%
    group_by(i) %>%
    summarize(avg_r_squared = mean(r_squared),
              avg_rmse = mean(rmse),
              avg_diff = mean(diff))
  
  return(n_shot_samples)
}




########## Graphing ########## 
# Graph RMSE
ggplot(results, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long FG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph Difference in FG%
ggplot(n_shot_samples, aes(x=i, y=avg_diff)) +
  geom_line() +
  xlab('# of shots') +
  ylab('Abs. Difference in FG%') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# To do: 2PFG%, 3PFG%, eFG%
