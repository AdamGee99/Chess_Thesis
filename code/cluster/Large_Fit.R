##### March 5th 2024 ######
###########
## Fit the current model to a large selection of 
## games on a cluster
## stan model is somewhat optimized for speed to fit to faster, using 
## matrix multiplication
##

library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)

options(mc.cores = parallel::detectCores())

## source helper functions for reading and transforming data
## along with defaults for plots, etc
#source(here("analysis/helper.R"))
#function to read in separate files and merge
read_player <- function(path, file){
  dat <- read_csv(file = paste0(data_path, file),
                  col_types = cols(UTCDate = col_date("%Y.%m.%d"),
                                   WhiteTitle = col_character(),
                                   BlackTitle = col_character(),
                                   WhiteElo = col_character(),
                                   BlackElo = col_character(),
                                   FEN = col_character())) %>%
    dplyr::select(Username, Event, White, Black, Result, UTCDate, UTCTime,
                  WhiteElo, BlackElo, Variant, TimeControl, Termination) %>%
    mutate(WhiteElo = parse_number(if_else(WhiteElo == "?", NA, WhiteElo)),
           BlackElo = parse_number(if_else(BlackElo == "?", NA, BlackElo)))
  dat
}


get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games %>% 
      filter(Username == user) %>%
      arrange(UTCDate, UTCTime) %>% 
      mutate(focal_white = ifelse(Username == White, 1, 0)) %>% 
      dplyr::select(White:BlackElo, focal_white) %>% 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) %>% 
      mutate(focal_win_prop = focal_result)
    
  }
  else{
    hist_games <- games %>% 
      filter(Username == user) %>%
      arrange(UTCDate, UTCTime) %>% 
      mutate(focal_white = ifelse(Username == White, 1, 0)) %>% 
      dplyr::select(White:BlackElo, focal_white) %>% 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) %>% 
      mutate(focal_win_prop = c(cumsum(focal_result[1:(prev_n - 1)])/(1:(prev_n -1)), 
                                roll_mean(focal_result, n = prev_n)))
  }
  
  hist_games
}

#path_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

set_cmdstan_path("/home/adamgee/R/x86_64-pc-linux-gnu-library/4.4/cmdstan")


### load in the data to use

data_path = "/home/adamgee/scratch/data/lichessGrandmasters/"
save_path = "/home/adamgee/scratch/results/"

#all_data_path <- rep(NA, 4)
#all_save_path <- rep(NA, 4)
#all_data_path[1] <- here("box_data/lichess1700-1900/")
#all_data_path[2] <- here("box_data/lichess2000-2200/")
#all_data_path[3] <- here("box_data/lichess2300-2500/")
#all_data_path[4] <- here("box_data/lichessGrandmasters/")
#all_save_path[1] <- here("results/lichess1700-1900_check/")

# all_save_path[1] <- here("results/Full_Fits/lichess1700-1900/")
# ## if need to run it locally

#all_save_path[2] <- here("results/lichess2000-2200_check/")
#all_save_path[3] <- here("results/lichess2300-2500_test/")
#all_save_path[4] <- here("results/lichessGrandmasters_check/")


#data_path <- all_data_path[path_id]
#save_path <- all_save_path[path_id]

#dir.create(save_path, showWarnings = FALSE)

files <- list.files(data_path)

lichess_data <- files %>% 
  map_dfr(~read_player(data_path, .x))

## restrict to rated rapid and shorter here
## this also removes the NAs, which makes sense

small_data <- lichess_data %>%
  mutate(Event = tolower(Event)) |>
  # filter(Event == "Rated Bullet game") %>%
  filter(TimeControl == "60+0") %>%
  filter(Variant == "Standard") %>%
  filter(grepl("rated bullet game", Event))

#remove duplicated games
small_data = small_data[!duplicated(small_data),]

# ## what time length should be
# small_data <- lichess_data %>%
#   mutate(Event = tolower(Event)) |>
#   filter(TimeControl == "180+0") %>%
#   filter(Variant == "Standard") %>%
#   filter(grepl("rated blitz game", Event))

rm(lichess_data)


## when players play less than 10 games
## otherwise not needed
users <- small_data %>%
  group_by(Username) %>%
  tally() %>%
  filter(n >= 10) %>%
  pull(Username)

# saveRDS(users, file = paste0(save_path, "users_bullet.RDS"))
# saveRDS(users, file = paste0(save_path, "users_blitz.RDS"))


tidy_games <- map_dfr(users, get_hist, small_data, prev_n = 1) %>% 
  as_tibble()

init_data <- tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  mutate(elo_diff = ifelse(focal_white == 1,
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  dplyr::select(focal_user, focal_id, focal_white,
         focal_win_prop, elo_diff, focal_result) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  filter(focal_result != 0.5)


### then fit the models

stan_data_ave <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$elo_diff,
                      win_prop = init_data$ave_prop)


#saveRDS(object = stan_data_ave, file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/gm_middle_1000_stan_data.RDS")


#stan_file <- here("owen", "cluster_scripts", "final_model_scale_priors.stan")
stan_file <- "/home/adamgee/scratch/code/final_model_scale_priors_NEW.stan"

mod <- cmdstan_model(stan_file)

fit3_ave <- mod$sample(data = stan_data_ave,
                       seed = 123,
                       chains = 4,
                       parallel_chains = 4,
                       refresh = 100)


## save the stan fit as not actually that large here,
## when no generated quantities

fit3_ave$save_object(file = "/home/adamgee/scratch/results/all_rated_prev_n1_gm_NEW.RDS")






