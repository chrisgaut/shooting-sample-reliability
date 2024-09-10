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
  ##       sample_name: Distinguish between different metrics being analyzed
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
              avg_diff = mean(diff),
              `FG%` = sample_name)
  
  # names(n_shot_samples)[names(n_shot_samples) == 'avg_rmse'] <- paste0('avg_rmse_', sample_name)
  
  return(n_shot_samples)
}

##############################


########## Analysis - Basic FG%s ########## 
## FG%
fg_pct <- shots

qual_shots_elbow_fg_pct <- qual_shot_elbow(fg_pct)

shot_min_fg_pct <- 300
n_samples_fg_pct <- 1000

results_fg_pct <- random_shot_samples(fg_pct, shot_min_fg_pct, n_samples_fg_pct, 'FG%')

## 2PFG%
fg_pct_2s <- shots %>% filter(SHOT_TYPE == '2PT Field Goal')

qual_shots_elbow_fg_pct_2s <- qual_shot_elbow(fg_pct_2s)

shot_min_fg_pct_2s <- 300
n_samples_fg_pct_2s <- 1000

results_fg_pct_2s <- random_shot_samples(fg_pct_2s, shot_min_fg_pct_2s, n_samples_fg_pct_2s, '2PFG%')

## 3PFG%
fg_pct_3s <- shots %>% filter(SHOT_TYPE == '3PT Field Goal')

qual_shots_elbow_fg_pct_3s <- qual_shot_elbow(fg_pct_3s)

shot_min_fg_pct_3s <- 300
n_samples_fg_pct_3s <- 1000

results_fg_pct_3s <- random_shot_samples(fg_pct_3s, shot_min_fg_pct_3s, n_samples_fg_pct_3s, '3PFG%')

## Combine
pcts_data <- rbind(results_fg_pct, results_fg_pct_2s, results_fg_pct_3s)

pcts_graph <- ggplot(pcts_data, aes(x=i, y=avg_rmse, color=`FG%`)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long FG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?') +
  geom_vline(xintercept=75, color = 'red', linetype='dashed') +
  geom_hline(yintercept=0.05, color = 'red', linetype='dashed') +
  geom_hline(yintercept=0.055, color = 'red', linetype='dashed')

##############################


########## Analysis - Ft. Distance FG%s ########## 
## Less Than 8 ft.
fg_pct_8_ft <- shots %>% filter(ZONE_RANGE == 'Less Than 8 ft.')

qual_shots_elbow_fg_pct_8_ft <- qual_shot_elbow(fg_pct_8_ft)

shot_min_fg_pct_8_ft <- 300
n_samples_fg_pct_8_ft <- 1000

results_fg_pct_8_ft <- random_shot_samples(fg_pct_8_ft, shot_min_fg_pct_8_ft, n_samples_fg_pct_8_ft, '<8 Ft. FG%')

## 8-16 ft.
fg_pct_8_16_ft <- shots %>% filter(ZONE_RANGE == '8-16 ft.')

qual_shots_elbow_fg_pct_8_16_ft <- qual_shot_elbow(fg_pct_8_16_ft)

shot_min_fg_pct_8_16_ft <- 300
n_samples_fg_pct_8_16_ft <- 1000

results_fg_pct_8_16_ft <- random_shot_samples(fg_pct_8_16_ft, shot_min_fg_pct_8_16_ft, n_samples_fg_pct_8_16_ft, '8-16 Ft. FG%')

## 16-24 ft.
fg_pct_16_24_ft <- shots %>% filter(ZONE_RANGE == '16-24 ft.')

qual_shots_elbow_fg_pct_16_24_ft <- qual_shot_elbow(fg_pct_16_24_ft)

## >24 ft.
fg_pct_24_ft <- shots %>% filter(ZONE_RANGE == '24+ ft.')

qual_shots_elbow_fg_pct_24_ft <- qual_shot_elbow(fg_pct_24_ft)

shot_min_fg_pct_24_ft <- 300
n_samples_fg_pct_24_ft <- 1000

results_fg_pct_24_ft <- random_shot_samples(fg_pct_24_ft, shot_min_fg_pct_24_ft, n_samples_fg_pct_24_ft, '>24 Ft. FG%')

## Combine
dist_data <- rbind(results_fg_pct_8_ft, results_fg_pct_8_16_ft, results_fg_pct_16_24_ft, results_fg_pct_24_ft)

dists_graph <- ggplot(dist_data, aes(x=i, y=avg_rmse, color=`FG%`)) +
  geom_line() +
  xlab('# of shots') +
  ylab('RMSE (Sample ~ Season-Long FG%)') +
  ggtitle('Elbow Jumper: When do shooting samples start to plateu?') +
  geom_vline(xintercept=75, color = 'red', linetype='dashed')

##############################


