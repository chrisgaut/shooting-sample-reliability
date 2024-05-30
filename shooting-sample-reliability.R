# Shooting sample reliability

# Load libraries
library(tidyverse)
library(Metrics)

# Load data
shots <- read_csv("NBA_2024_Shots.csv")

# Drop unnecessary columns
shots2 <- shots %>%
  select(-SEASON_1, -SEASON_2, -TEAM_ID, -PLAYER_ID, -GAME_ID, -HOME_TEAM, 
         -AWAY_TEAM, -EVENT_TYPE, -ZONE_NAME, -ZONE_ABB, -LOC_X, -LOC_Y)

# Format dates
shots3 <- shots2 %>%
  mutate(GAME_DATE = as.Date(shots2$GAME_DATE, format="%m-%d-%Y"))

# R-Squared function
rsq <- function (x, y) cor(x, y) ^ 2


################# FG% ################# 
# Number of shots
fg <- shots3 %>%
  group_by(PLAYER_NAME) %>% mutate(shot_num = row_number())

# Get list of players with at least 500 shot attempts
qualifying_players_fg <- fg %>%
  filter(shot_num >= 1000) %>%
  select(PLAYER_NAME) %>%
  unique()

# Filter data on qualifying players
shots4 <- shots3 %>% filter(PLAYER_NAME %in% qualifying_players_fg$PLAYER_NAME)

# Get qualified players' season-long FG%s
season_long_pct <- shots4 %>%
  group_by(PLAYER_NAME) %>% 
  summarize(season_fg_pct = sum(SHOT_MADE)/n())

all_samples_r_squared <- data.frame()
# Run 100 random samples for correlation smoothing
for(i in seq(from=1, to=100, by=1)){
  # Create n-shot random samples for each player, calculate %s
  random_n_fga <- data.frame()
  
  for(i in seq(from=10, to=500, by=10)){
    random_shots <- shots4 %>% 
      group_by(PLAYER_NAME) %>% 
      slice_sample(n=i) %>% 
      summarize(n_shots = i,
                sample_fg_pct = sum(SHOT_MADE)/i)
    
    random_n_fga <- rbind(random_n_fga, random_shots)
  }
  
  # Join season-long FG%s onto random samples
  random_n_fga <- random_n_fga %>%
    inner_join(season_long_pct, by = c(PLAYER_NAME = "PLAYER_NAME"))
  
  
  # Get R-Squared values by n-shots
  n_shots_r_squared <- data.frame()
  
  for(i in seq(from=10, to=500, by=10)){
    subset_for_r_squared <- random_n_fga %>% 
      filter(n_shots == i)
    
    r_squared = rsq(subset_for_r_squared$sample_fg_pct, subset_for_r_squared$season_fg_pct)
    
    rmse = rmse(subset_for_r_squared$sample_fg_pct, subset_for_r_squared$season_fg_pct)
    
    diff = abs(subset_for_r_squared$sample_fg_pct - subset_for_r_squared$season_fg_pct)
    
    df = data.frame(i, r_squared, rmse, diff)
    
    n_shots_r_squared <- rbind(n_shots_r_squared, df)
  }

  all_samples_r_squared <- rbind(all_samples_r_squared, n_shots_r_squared)
}

n_shot_samples <- all_samples_r_squared %>%
  group_by(i) %>%
  summarize(avg_r_squared = mean(r_squared),
            avg_rmse = mean(rmse),
            avg_diff = mean(diff))

# Graph R squareds
ggplot(n_shot_samples, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long FG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')
  

# To do: Create R Shiny app where you can change metrics, sample sizes, etc.
