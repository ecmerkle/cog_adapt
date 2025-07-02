score_bayes_update <- function(datasets_to_save) {

  library(tidyverse)
  
  bayesian_update_data <- datasets_to_save$bayesian_update_data
  
  bayesian_update_data <- bayesian_update_data %>%
  # filter(row_number()<6) %>%
  # select(ball_split, version, current_draw, past_draws, response_slider, right_box_majority_color, score) %>%
  mutate(
    combined_draws = ifelse(is.na(past_draws), as.character(current_draw), paste(past_draws, as.character(current_draw), sep = ",")),
    blue_balls = str_count(combined_draws, "blue"),
    red_balls = str_count(combined_draws, "red"),
    
    # Calculate probabilities
    ball_split_numeric = str_split(ball_split, ",") %>% map(~ as.numeric(.x) / 100),
    probability_blue_ball = map_dbl(ball_split_numeric, 2),
    probability_red_ball = map_dbl(ball_split_numeric, 1),
    true_odds_blue_urn = (probability_blue_ball / probability_red_ball) ** (blue_balls - red_balls),
    
    response_slider_modified = case_when(
      response_slider==0 ~ 1,
      response_slider==100 ~ 99,
      TRUE ~ response_slider
    ),
    # Calculate reported probability and true odds based on version
    reported_probability = if_else(
      version == "easy",
      if_else(right_box_majority_color == "blue", response_slider_modified / 100, (100 - response_slider_modified) / 100),
      response_slider_modified / 100
    ),
    true_odds = if_else(
      version == "easy",
      true_odds_blue_urn,
      {
        true_probability_blue_urn = true_odds_blue_urn / (1 + true_odds_blue_urn)
        true_probability_blue_ball = (true_probability_blue_urn * probability_blue_ball) + ((1 - true_probability_blue_urn) * (1 - probability_blue_ball))
        true_probability_blue_ball / (1 - true_probability_blue_ball)
      }
    ),
    # Calculate reported odds
    reported_odds = reported_probability / (1 - reported_probability),
    # Calculate the score
    score2 = abs(log(reported_odds) - log(true_odds))
    #)
  ) %>%
    select(-c(combined_draws, blue_balls, red_balls, ball_split_numeric, probability_blue_ball, probability_red_ball, true_odds_blue_urn, response_slider_modified, reported_probability, true_odds, reported_odds))

  bayesian_update_data

  ## TODO limit to people in datasets_to_save$scores, using metadata to match subject id
}

if (FALSE) {
  ## PILOT
  load("data/FPT_datasets.RData")
  tmp <- score_bayes_update(datasets_to_save)
  ## mean by hard/easy per person
  mns <- with(tmp, tapply(score2, list(session_id, version), mean, na.rm = TRUE))
  with(tmp, tapply(score2, version, summary))

  ## MAIN
  budat <- read.csv("data/main_study_25oct2024/20241025/bayesian_update_data.csv")
  ## mean by hard/easy per person
  mns <- with(budat, tapply(score2, list(session_id, version), mean, na.rm = TRUE))
  with(budat, tapply(score2, version, summary))
}
