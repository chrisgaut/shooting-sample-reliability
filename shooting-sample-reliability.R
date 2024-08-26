# Shooting sample reliability

########## Analysis Prep ########## 
# Load libraries
library(tidyverse)
library(Metrics)

# Load data
shots <- read_csv("NBA_2024_Shots.csv")

##############################


########## Qualifying Shots Elbow Chart Function ########## 
qual_shot_elbow <- function(data) { 
  ########################
  ## qual_shot_elbow: Charts the number of players that have taken n-shot attempts
  ## Args:
  ##.      data: NBA Shots data frame
  ########################
  shot_attempts <- data %>%
    group_by(PLAYER_NAME) %>% 
    mutate(shot_num = row_number())
  
  count_by_shot_attempts <- shot_attempts %>%
    group_by(shot_num) %>%
    mutate(count_players = n()) %>%
    select(shot_num, count_players) %>%
    unique()
  
  qual_shot_elbow_chart <- ggplot(count_by_shot_attempts, aes(x=shot_num, y=count_players)) +
    geom_line() +
    xlab('# of shots') +
    ylab('# of players')
  
  return(qual_shot_elbow_chart)
}

##############################


########## Random Sampling + Smoothing Function ########## 
random_shot_samples <- function(data, shot_min, n_samples, sample_name) { 
  ########################
  ## random_shot_samples: Takes i-iterations of 10-interval random shot samples, based on q-qualifying number of shots.
  ## Args:
  ##.      data: NBA Shots data frame
  ##       shot_min: Qualifying number of shots to filter players on
  ##       n_samples: Number of samples taken for smoothing
  ########################
  
  
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
  
  # Run i random samples for correlation smoothing
  all_samples_metrics <- data.frame()
  for(i in seq(from=1, to=n_samples, by=1)){
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
      
      rmse = rmse(subset_for_metrics$sample_pct, subset_for_metrics$season_pct)
      
      diff = abs(subset_for_metrics$sample_pct - subset_for_metrics$season_pct)
      
      app_df = data.frame(i, rmse, diff)
      
      n_shots_accuracy <- rbind(n_shots_accuracy, app_df)
    }
    
    all_samples_metrics <- rbind(all_samples_metrics, n_shots_accuracy)
  }
  
  # Average accuracy metrics across all random samples
  n_shot_samples <- all_samples_metrics %>%
    group_by(i) %>%
    summarize(avg_rmse = mean(rmse),
              avg_diff = mean(diff))
  
  names(n_shot_samples)[names(n_shot_samples) == 'avg_rmse'] <- paste0('avg_rmse_', sample_name)
  
  return(n_shot_samples)
}

##############################


########## Analysis ########## 
## FG%
fg_pct <- shots

qual_shots_elbow_fg_pct <- qual_shot_elbow(fg_pct)

shot_min_fg_pct <- 500
n_samples_fg_pct <- 1000

results_fg_pct <- random_shot_samples(fg_pct, shot_min_fg_pct, n_samples_fg_pct, 'fg')

## FG% - 2
shot_min_fg_pct2 <- 250
n_samples_fg_pct2 <- 200

results_fg_pct2 <- random_shot_samples(fg_pct, shot_min_fg_pct2, n_samples_fg_pct2)

## 2PFG%
fg_pct_2s <- shots %>% filter(SHOT_TYPE == '2PT Field Goal')

qual_shots_elbow_fg_pct_2s <- qual_shot_elbow(fg_pct_2s)

shot_min_fg_pct_2s <- 250
n_samples_fg_pct_2s <- 200

results_fg_pct_2s <- random_shot_samples(fg_pct_2s, shot_min_fg_pct_2s, n_samples_fg_pct_2s)

## 3PFG%
fg_pct_3s <- shots %>% filter(SHOT_TYPE == '3PT Field Goal')

qual_shots_elbow_fg_pct_3s <- qual_shot_elbow(fg_pct_3s)

shot_min_fg_pct_3s <- 250
n_samples_fg_pct_3s <- 200

results_fg_pct_3s <- random_shot_samples(fg_pct_3s, shot_min_fg_pct_3s, n_samples_fg_pct_3s)

## Less Than 8 ft.
fg_pct_8_ft <- shots %>% filter(ZONE_RANGE == 'Less Than 8 ft.')

qual_shots_elbow_fg_pct_8_ft <- qual_shot_elbow(fg_pct_8_ft)

shot_min_fg_pct_8_ft <- 250
n_samples_fg_pct_8_ft <- 1000

results_fg_pct_8_ft <- random_shot_samples(fg_pct_8_ft, shot_min_fg_pct_8_ft, n_samples_fg_pct_8_ft)

## 8-16 ft.
fg_pct_8_16_ft <- shots %>% filter(ZONE_RANGE == '8-16 ft.')

qual_shots_elbow_fg_pct_8_16_ft <- qual_shot_elbow(fg_pct_8_16_ft)

shot_min_fg_pct_8_16_ft <- 250
n_samples_fg_pct_8_16_ft <- 1000

results_fg_pct_8_16_ft <- random_shot_samples(fg_pct_8_16_ft, shot_min_fg_pct_8_16_ft, n_samples_fg_pct_8_16_ft)

## 16-24 ft.
fg_pct_16_24_ft <- shots %>% filter(ZONE_RANGE == '16-24 ft.')

qual_shots_elbow_fg_pct_16_24_ft <- qual_shot_elbow(fg_pct_16_24_ft)

shot_min_fg_pct_16_24_ft <- 250
n_samples_fg_pct_16_24_ft <- 1000

results_fg_pct_16_24_ft <- random_shot_samples(fg_pct_16_24_ft, shot_min_fg_pct_16_24_ft, n_samples_fg_pct_16_24_ft)

## >24 ft.
fg_pct_24_ft <- shots %>% filter(ZONE_RANGE == '24+ ft.')

qual_shots_elbow_fg_pct_24_ft <- qual_shot_elbow(fg_pct_24_ft)

shot_min_fg_pct_24_ft <- 250
n_samples_fg_pct_24_ft <- 1000

results_fg_pct_24_ft <- random_shot_samples(fg_pct_24_ft, shot_min_fg_pct_24_ft, n_samples_fg_pct_24_ft)

##############################


########## Graph Results ########## 
# Graph RMSE for FG%
elbow_fg <- ggplot(results_fg_pct, aes(x=i, y=avg_rmse_fg)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long FG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for FG% - 2
elbow_fg2 <- ggplot(results_fg_pct2, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long FG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for 2PFG%
elbow_2s <- ggplot(results_fg_pct_2s, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long 2PFG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for 3PFG%
elbow_3s <- ggplot(results_fg_pct_3s, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long 3PFG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for <8ft%
elbow_8_ft <- ggplot(results_fg_pct_8_ft, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long <8ftFG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for 8-16ft%
elbow_8_16_ft <- ggplot(results_fg_pct_8_16_ft, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long 8-16ftFG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for 8-16ft%
elbow_16_24_ft <- ggplot(results_fg_pct_16_24_ft, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long 16-24ftFG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

# Graph RMSE for >24ft%
elbow_24_ft <- ggplot(results_fg_pct_24_ft, aes(x=i, y=avg_rmse)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long >24ftFG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?')

##############################

# To Do
# 3. Combine data frames to draw graphs on top of each other
#     - Add 'suffix' parameter to sampling function
# 4. Begin analysis, working on markdown