library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(rstanarm)
library(RcppRoll)
library(data.table)
library(MASS)
library(scales)
library(ggridges)
library(latex2exp)
library(ggdist)


### IMPORTING MODELS ###

#these are model results for bullet games, taking only 1 game as history, for 1700-1900s and GMs

amateur_model = readRDS("C:/Users/adamg/Desktop/Thesis Fall 2024/CASSIS Poster/code stuff for getting figures/all_rated_bullet_model_prev_amateurs.RDS")

gm_model = readRDS("C:/Users/adamg/Desktop/Thesis Fall 2024/CASSIS Poster/code stuff for getting figures/all_rated_bullet_model_prev_gms.RDS")

gm_model = readRDS("C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/GM_middle_1000_game_gen.RDS")

#saveRDS(gm_model_summary, file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/gm_middle_1000_summary.RDS")

amateur_model_summary = amateur_model$summary()
gm_model_summary = gm_model$summary()





### IMPORTING DATA ###

### FUNCTIONS ###

#function to read in separate files and merge
read_player = function(path, file){
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

### 1700-1900 ###
data_path = "C:/Users/adamg/Desktop/Thesis/data/lichess1700-1900/"

files = list.files(data_path)

lichess_data_amateur = files %>%
  map_dfr(~read_player(data_path, .x))

#selecting bullet 60+0 only
bullet_60_amateur = lichess_data_amateur %>% 
  filter(Event == "Rated Bullet game") %>% 
  filter(TimeControl == "60+0") %>%
  dplyr::select(c("White", "Black", "Result", "UTCDate", "UTCTime", "Username", "WhiteElo", "BlackElo"))


### GMS ###

data_path = "C:/Users/adamg/Desktop/Thesis/data/lichessGrandmasters/"
files <- list.files(data_path)

lichess_data_gms = files %>%
  map_dfr(~read_player(data_path, .x))

#selecting bullet 60+0 only
bullet_60_gms = lichess_data_gms %>% 
  filter(Event == "Rated Bullet game") %>% 
  filter(TimeControl == "60+0") %>%
  dplyr::select(c("White", "Black", "Result", "UTCDate", "UTCTime", "Username", "WhiteElo", "BlackElo"))



amateur_players = readRDS(file = "users_bullet_amateur.RDS")
gm_players = readRDS(file = "users_bullet_gm.RDS")





##### PLOTTING ####





#alpha draws
mcmc_hist(amateur_model$draws(paste0("beta[1,", 1:25, "]") )) 

#beta draws
mcmc_hist(amateur_model$draws(paste0("beta[2,", 1:25, "]") )) 









### Global Parameters

axis_title <- 16
title_size <- 18
axis_text_size <- 14
legend_text <- 14
line_size <- 1
theme_4col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(0.5, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 2),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 5, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}

theme_3col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(1, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 1.5),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 4, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}
# color_scheme_set("blue")

## easiest way to make this grayscale would be to not use mcmc_hist at all

global_17_19_bullet_prev <- amateur_model$draws(c("mu_beta",
                                                          "gamma1", "gamma2",
                                                          "sigma_1", "tau[1]", "tau[2]",
                                                          "sigma_g1", "sigma_g2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 3, scales = "free") +
  labs(x = element_blank(), y = element_blank()) +
  theme_3col() +
  scale_x_continuous(breaks = breaks_pretty(n = 2))


global_17_19_bullet_prev

ggsave(plot = global_17_19_bullet_prev, 
       filename = "C:/Users/adamg/Desktop/CASSIS Poster/amateur_global_effects.png", dpi = 1000,
       height = 4, width = 7)

## GMS


global_gm_bullet_prev <- gm_model$draws(c("mu_beta",
                                                  "gamma1", "gamma2",
                                                  "sigma_1", "tau[1]", "tau[2]",
                                                  "sigma_g1", "sigma_g2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 4, scales = "free") +
  labs(x = element_blank(), y = element_blank()) +
  theme_3col() +
  scale_x_continuous(breaks = breaks_pretty(n = 2))


global_gm_bullet_prev

ggsave(plot = global_gm_bullet_prev, 
       filename = "C:/Users/adamg/Desktop/CASSIS Poster/gm_global_effects.png", dpi = 1000,
       height = 4, width = 7)




### PLAYER EFFECTS ###



## AMATEURS

## need to get the write names then for these parameters here
## did this somewhere before already

player_id <- as_tibble(amateur_players) |> 
  mutate(id = row_number()) |> 
  rename(player_name = value)


player_pars <- amateur_model$draws() %>% 
  as_draws_df() %>%
  dplyr::select(starts_with("beta[")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param)) |> 
  left_join(player_id, by = "id")



#players to highlight individual effects in poster
poster_amateur_players = c("APlayerfromEarth", "beirut2010", "larrywheels", "ptk77")


## then the winner effects plot
winner_plot_17_19 <- player_pars |> 
  filter(player_name %in% poster_amateur_players) %>%
  filter(param == 2) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~player_name, scales = "free", ncol = 6) +
  theme_4col() +
  labs(x = element_blank(), y = element_blank()) +
  scale_x_continuous(breaks = breaks_pretty(n = 3))


winner_plot_17_19


ggsave(plot = winner_plot_17_19, 
       filename = "C:/Users/adamg/Desktop/CASSIS Poster/amateur_winner_effects.png", dpi = 1000,
       height = 4, width = 7)


## then repeat for player effects

indiv_plot_17_19 <- player_pars |> 
  filter(param == 1) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~player_name, scales = "free", ncol = 6) +
  theme_4col() +
  labs(x = element_blank(), y = element_blank()) +
  scale_x_continuous(breaks = breaks_pretty(n = 3))


indiv_plot_17_19


#ggsave(plot = indiv_plot_17_19, 
#       filename = paste0(here("Paper_WriteUp", "paper_figures",
#                              "17_19_prev_indiv.png")), dpi = 600,
#       height = 7, width = 7)






## GMs

## need to get the write names then for these parameters here
## did this somewhere before already

player_id <- as_tibble(gm_players) |> 
  mutate(id = row_number()) |> 
  rename(player_name = value)


player_pars <- gm_model$draws() %>% 
  as_draws_df() %>%
  dplyr::select(starts_with("beta[")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param)) |> 
  left_join(player_id, by = "id")



#players to highlight individual effects in poster
poster_gm_players = c("alireza2003", "DrNykterstein", "nihalsarin2004", "Zhigalko_Sergei")


## then the winner effects plot
winner_plot_gm <- player_pars |> 
  filter(player_name %in% poster_gm_players) %>%
  filter(param == 2) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~player_name, scales = "free", ncol = 6) +
  theme_4col() +
  labs(x = element_blank(), y = element_blank()) +
  scale_x_continuous(breaks = breaks_pretty(n = 3))


winner_plot_gm


ggsave(plot = winner_plot_gm, 
       filename = "C:/Users/adamg/Desktop/CASSIS Poster/gm_winner_effects.png", dpi = 1000,
       height = 4, width = 7)


## then repeat for player effects

indiv_plot_17_19 <- player_pars |> 
  filter(param == 1) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~player_name, scales = "free", ncol = 6) +
  theme_4col() +
  labs(x = element_blank(), y = element_blank()) +
  scale_x_continuous(breaks = breaks_pretty(n = 3))


indiv_plot_17_19


#ggsave(plot = indiv_plot_17_19, 
#       filename = paste0(here("Paper_WriteUp", "paper_figures",
#                              "17_19_prev_indiv.png")), dpi = 600,
#       height = 7, width = 7)








#### PPDs ####





## just do ppc plots for now

fit_gm_first <- readRDS(file = "C:/Users/adamg/Desktop/CASSIS Poster/code stuff for getting figures/select_users_first_bullet_gm.RDS")
fit_gm_last <- readRDS(file =  "C:/Users/adamg/Desktop/CASSIS Poster/code stuff for getting figures/select_users_last_bullet_gm.RDS")

fit_gm_middle_1000 = gm_model

fit_samples <- as_draws_df(fit_gm_middle_1000$draws())

fit_samp <- fit_samples %>% 
  dplyr::select(!starts_with(c("log_lik", "yrep")))

y_rep <- fit_samples %>% dplyr::select(starts_with("y_rep"))

rm(fit_samples)
rm(fit_samp)

stan_data_ave_middle_1000 <- readRDS(file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/gm_middle_1000_stan_data.RDS")

y_rep_mod <- y_rep %>% 
  rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>% 
  ## remove the draws and chains here
  mutate(focal_id = stan_data_ave_middle_1000$id)

games_won <- y_rep_mod %>% 
  pivot_longer(cols = `1`:`4000`, names_to = "draw", values_to = "y") %>% 
  group_by(focal_id, draw) %>% 
  summarise(games_won = sum(y)) 

orig_data <- tibble(outcome = stan_data_ave_middle_1000$y,
                    focal_id = stan_data_ave_middle_1000$id)

users <- readRDS(file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/users_bullet.RDS")

#for poster
#users = users[1:5]

## need to give these the correct names here
player_id <- tibble(id = as.character(1:length(users)), player = users)

orig_games_won <- orig_data %>% 
  group_by(focal_id) %>% 
  summarise(games_won = sum(outcome)) %>% 
  mutate(focal_id = as.character(focal_id)) %>% 
  left_join(player_id, by = c("focal_id" = "id"))


p1 <- games_won %>% 
  mutate(focal_id = as.character(focal_id)) %>% 
  left_join(player_id, by = c("focal_id" = "id")) %>% 
  ggplot(aes(x = games_won)) +
  geom_histogram() +
  facet_wrap(~player, scales = "free", ncol = 5) +
  geom_vline(data = orig_games_won, 
             mapping = aes(xintercept = games_won), col = "red") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  # labs(title = "Posterior Predictive Distribution, Initial Games",
  labs(y = element_blank(),
       x = "Number of Games Won") +
  theme_4col() +
  theme(axis.title.x = element_text(size = 10,
                                    margin = margin(t = 10, b = 10, unit = "pt")),
        panel.spacing.x = unit(0.7, "lines"))

p1

ggsave(plot = p1, 
       filename = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/ppc_mid_1000_fit.png", dpi = 1000,
       height = 6.5, width = 9)




### now test on final 1000 games



draws <- fit_gm_middle_1000$draws() %>% as_draws_df() %>%
  dplyr::select(starts_with(c("beta[", "gamma"))) %>%
  mutate(draw = row_number()) %>% 
  pivot_longer(cols = "beta[1,1]":"beta[2,10]") %>% 
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = param, values_from = value) %>% 
  rename(beta1 = `1`, beta2 = `2`)

stan_data_ave_last <- readRDS(file = "C:/Users/adamg/Desktop/Thesis Fall 2024/CASSIS Poster/code stuff for getting figures/stan_data_last_gm.RDS")

future_games <- tibble(focal = as.character(stan_data_ave_last$id), 
                       color = stan_data_ave_last$colour, 
                       elo_diff = stan_data_ave_last$elo,
                       hist = stan_data_ave_last$win_prop)

ppc_data <- readRDS(file = "C:/Users/adamg/Desktop/Thesis Fall 2024/CASSIS Poster/code stuff for getting figures/ppc_fit_first_last_gm.RDS")

future_results <- tibble(focal = as.character(stan_data_ave_last$id),
                         result = stan_data_ave_last$y) %>% 
  group_by(focal) %>%
  summarise(total = sum(result)) %>% 
  rename(id = focal) %>% 
  left_join(player_id, by = "id")

last_ppc <- ppc_data %>%  
  left_join(player_id, by = "id") %>% 
  ggplot(aes(x = games_won)) + 
  geom_histogram() + 
  geom_vline(data = future_results, 
             mapping = aes(xintercept = total), col = "red") +
  facet_wrap(~player, scales = "free_x", ncol = 5) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(y = element_blank(), x = "Number of Games Won") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_4col() +
  theme(axis.title.x = element_text(size = 10,
                                    margin = margin(t = 10,
                                                    b = 10,
                                                    unit = "pt")))

last_ppc

ggsave(plot = last_ppc, 
       filename ="C:/Users/adamg/Desktop/CASSIS Poster/ppc_2_gm.png", dpi = 1000,
       height = 4, width = 9)












#### ELO PLOTS ####



#plotting elo vs date for two players who started playing at roughly the same time


theme_1col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(1, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 1),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 3, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      #axis.text.y = element_blank(),
      #axis.ticks.y = element_blank()
    )
}

#theme_set(theme_bw())
theme_set(theme_1col())

#players = c("Elfugitivo", "beirut2010")
#players = unique(bullet_60_gms$Username)

players = c("DrNykterstein", "nihalsarin2004", "alireza2003",
            "d8336", "larrywheels")

#players = unique(bullet_60_gms$Username)

elo_plot = bullet_60_gms %>%
  rbind(bullet_60_amateur) %>%
  mutate(UTCDateTime = ymd_hms(paste(UTCDate, UTCTime))) %>%
  mutate(elo = ifelse(White == Username, WhiteElo, BlackElo)) %>%
  filter(Username %in% players) %>% 
  dplyr::select(Username, UTCDateTime, elo) %>%
  mutate(Username = as.factor(Username))


elo_history_plot = ggplot(data = elo_plot, aes(x = UTCDateTime, y = elo, colour = Username)) +
  geom_line(linewidth = 1) +
  xlab("Year") +
  ylab("Glicko2 Rating")

elo_history_plot

ggsave(plot = elo_history_plot, 
       filename = "C:/Users/adamg/Desktop/joint seminar - Fall 2024/figures/elo_histroy_select_players.png",
       device = "png", dpi = 1000,
       width = 9, height = 4.5)









#### PPD ELO plots


#also fixing issue of using true ELO in the win PPDs




#### Sept 18th 2024
#### Construc a PPC of the evolution of the Lichess rating over time
#### Using complete simulated trajectories of their final 1000 games,
#### using parameter estimats from model fit to first 1000 games

library(PlayerRatings)
library(tidyverse)
library(here)
library(loo)
library(ggplot2)
library(posterior)
library(RcppRoll)

#source(here("analysis/helper.R"))

## get some initial chess data, say the final 1000 games
## for 20 players and compute the ratings for that


get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games %>% 
      filter(White == user | Black == user) %>% 
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
      filter(White == user | Black == user) %>% 
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


# all_data_path <- rep(NA, 4)
# all_save_path <- rep(NA, 4)
# all_data_path[1] <- here("box_data/lichess1700-1900/")
# all_data_path[2] <- here("box_data/lichess2000-2200/")
# all_data_path[3] <- here("box_data/lichess2300-2500/")
# all_data_path[4] <- here("box_data/lichessGrandmasters/")
# all_save_path[1] <- here("results/lichess1700-1900_test/")

# all_save_path[1] <- here("results/Full_Fits/lichess1700-1900/")
# ## if need to run it locally

# all_save_path[2] <- here("results/lichess2000-2200_test/")
# all_save_path[3] <- here("results/lichess2300-2500_test/")
# all_save_path[4] <- here("results/lichessGrandmasters_test/")
# 
# path_id <- 1  ## setting it for now
# data_path <- all_data_path[path_id]
# save_path <- paste0(all_save_path[path_id], "perm/")


# res_data_path <- rep(NA, 4)
# res_data_path[1] <- here("results/lichess1700-1900/")
# res_data_path[2] <- here("results/lichess2000-2200/")
# res_data_path[3] <- here("results/lichess2300-2500/")
# res_data_path[4] <- here("results/lichessGrandmasters/")


# files <- list.files(data_path)
# 
# lichess_data <- files |> 
#   map_dfr(~read_player(data_path, .x))


small_data <- lichess_data_gms |>
  mutate(Event = tolower(Event)) |> 
  filter(TimeControl == "60+0") |>
  filter(Variant == "Standard") |>
  filter(grepl("rated bullet game", Event))

#rm(lichess_data)

select_users <- small_data |> 
  group_by(Username) |> 
  tally() |> 
  arrange(-n) |> 
  #slice_max(order = n, n = 10) |> 
  pull(Username)


users <- select_users


## will use last 1000 games each user plays

last_games <- small_data |> 
  filter(Username %in% users) |> 
  group_by(Username) |> 
  arrange(UTCDate, UTCTime, .by_group = TRUE) |> 
  slice_tail(n = 1000) |> 
  ungroup() #|> 
  #group_by(Username) |> 
  #mutate(Result = sample(Result)) |> 
  #ungroup()

#why do we shuffle (smaple) the results here?

#last_games %>% mutate(focal_elo = ifelse(Username == White, WhiteElo, BlackElo)) %>% 
#  filter(Username == users[13]) %>% arrange(UTCDate, UTCTime) %>% 
#  mutate(win_loss = ifelse( (Result == "1-0" & Username == White) | (Result == "0-1" & Username == Black), "win", ifelse(Result == "1/2-1/2", "draw", "loss") ))

tidy_games <- map_dfr(users, get_hist, last_games, prev_n = 1) |> 
  as_tibble()



### THE FOCAL RESULT IN THESE GAMES IS WRONG, 
#filter for magnus, see that in his first games it says he loses all of them but his elo is going up
#probably because focal result was mutated in with the wrong date

#this was because the game results were being shuffled earlier on, I removed this and all is good



init_data <- tidy_games |> 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) |> 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) |> 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) |> 
  mutate(focal_id = match(focal_user, users)) |> 
  group_by(focal_id)  |> 
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_win_prop)) |>
  arrange(UTCDate, UTCTime) %>%
  filter(focal_result != 0.5)




## get their starting scores for each player



## then just pick the player from this data
## need to choose the focal id and format for the glicko2 function

focal_player_used <- 13

focal_init <- init_data |> group_by(focal_user) |> 
  slice_head(n = 1) |> 
  arrange(focal_id) |> 
  filter(focal_id == focal_player_used) |> 
  mutate(focal_elo = ifelse(focal_user == White, WhiteElo, BlackElo)) |> 
  dplyr::select(focal_user, focal_elo) |> 
  rename(Player = focal_user, Rating = focal_elo)



glick_data <- init_data |>
  filter(focal_user == users[focal_player_used])


#### remove duplicated rows, not sure why this is happening
#glick_data = glick_data[!duplicated(glick_data),]

#this doesnt seem to change anything


# First compare computed Glicko with Lichess ------------------------------


## first argument to glicko2
glick_part1 <- glick_data |> 
  arrange(UTCDate, UTCTime) |> 
  mutate(index = row_number()) |> 
  mutate(opp_elo = ifelse(focal_user == White, BlackElo, WhiteElo),
         opp = ifelse(focal_user == White, Black, White)) |> 
  dplyr::select(index, focal_user, opp, focal_result, focal_white) |> 
  mutate(focal_white = ifelse(focal_white == 0, -1, focal_white))




## second argument, giving elo of all opponents and starting of first
glick_part2 <- glick_data |> 
  arrange(UTCDate, UTCTime) |> 
  mutate(index = row_number()) |> 
  mutate(opp_elo = ifelse(focal_user == White, BlackElo, WhiteElo),
         opp = ifelse(focal_user == White, Black, White)) |> 
  dplyr::select(opp, opp_elo) |> 
  rename(Player = opp, Rating = opp_elo) 

## need to select the Deviation (to start with) and Volatility 

#looking at 

glick_part2 <- bind_rows(focal_init, glick_part2) |> 
  mutate(Deviation = 50, Volatility = 0.04)

initstate <- glick_part2

games <- data.frame(Week = glick_part1$index,
                    Player1 = focal_init$Player,
                    Player2 = glick_part1$opp,
                    Score = glick_part1$focal_result)

a <- glicko2(games, status = initstate, history = TRUE, 
             gamma = glick_part1$focal_white)
a
focal_glick <- a$history[focal_init$Player, ,]

glick_scores <- as.numeric(focal_glick[, "Rating"])

plot(glick_scores, type = "l")

## plot against lichess estimates

true_elo = glick_data %>%
  mutate(focal_elo = ifelse(White == users[focal_player_used], WhiteElo, BlackElo)) %>%
  mutate(index = row_number()) %>%
  mutate(Version = "Truth") %>%
  dplyr::select(index, focal_elo, Version)


true_elo |> 
  bind_rows(
    tibble(focal_elo = glick_scores, Version = "Est") |> 
      mutate(index = row_number())
  ) |> 
  ggplot(aes(index, focal_elo, colour = Version)) +
  geom_line()



# Setup to Simulate Complete Trajectory of Games and Scores ---------------------------------------

#data_path <- res_data_path[path_id]


## get the draws from the first
#fit_17_19_first <- readRDS(file =  "C:/Users/adamg/Desktop/CASSIS Poster/code stuff for getting figures/select_users_first_bullet_gm.RDS")

#fit_17_19_last <- readRDS(file =  "C:/Users/adamg/Desktop/CASSIS Poster/code stuff for getting figures/select_users_last_bullet_gm.RDS")


fit_gm_middle_1000 = gm_model


## extract draws from model fit to first

draws <- fit_gm_middle_1000$draws() |> as_draws_df() |>
  dplyr::select(starts_with(c("beta[", "gamma"))) |>
  mutate(draw = row_number()) |> 
  pivot_longer(cols = "beta[1,1]":"beta[2,23]") |> 
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  dplyr::select(-name) |> 
  pivot_wider(names_from = param, values_from = value) |> 
  rename(beta1 = `1`, beta2 = `2`)


## the covariates for the future games

future_games <- tibble(focal = as.character(init_data$focal_id),
                       color = init_data$focal_white,
                       elo_diff = init_data$elo_diff,
                       hist = init_data$ave_prop)


## process for below

focal_games <- future_games |> 
  filter(focal == focal_player_used)


full_game_info <- init_data |> 
  filter(focal_id == focal_player_used)

# slow for even 100 sims at the moment, due to the for loop
num_sims <- 100

num_games <- nrow(full_game_info)

glicko_sim <- data.frame(NA, nrow = num_games, ncol = num_sims)

game_sim <- data.frame(NA, nrow = num_games, ncol = num_sims)


## store the true overall average
## focal_win_prop = focal_result when just using previous game
true_avg <- mean(full_game_info$focal_result)

focal_draws <- draws |> 
  filter(id == focal_player_used)

for(i in 1:num_games){
  
  ## get the game information for the ith game
  game_info <- focal_games[i, ]
  # gamma the colour effect for glicko calc
  gamma <- ifelse(game_info$color == 1, 1, -1)
  
  # get opponent name and ability
  opp_elo <- full_game_info[i,] |> 
    mutate(opp = ifelse(focal_white == 1, BlackElo, WhiteElo)) |> 
    pull(opp)
  opp <- full_game_info[i,] |> 
    mutate(opp_name = ifelse(focal_white == 1, Black, White)) |> 
    pull(opp_name)
  
  curr_game <- draws[]
  
  for(j in 1:num_sims){
    
    curr_draw <- focal_draws[focal_draws$draw == j, ]
    
    ## if i = 1 use the initial glicko, otherwise use the previous
    if(i == 1){
      curr_glick <- focal_init$Rating
      curr_prop <- 0
    }else{
      curr_glick <- glicko_sim[i-1, j]
      curr_prop <- game_sim[i-1, j] - true_avg ## take lagged win rate
    }
    ## then simulate the outcome and update the glicko
    inv_logit <- curr_draw$beta1 + curr_draw$beta2 * curr_prop +
      curr_draw$gamma1 * game_info$color + 
      curr_draw$gamma2 * (curr_glick - opp_elo)
    prob <- exp(inv_logit)/(1 + exp(inv_logit))
    ## get sim result
    sim_result <- sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))
    game_sim[i, j] <- sim_result
    games <- data.frame(Week = i,
                        Player1 = focal_init$Player,
                        Player2 = opp,
                        Score = sim_result)
    initstate <- tibble(Player = c(focal_init$Player, opp),
                        Rating = c(curr_glick, opp_elo),
                        Deviation = 50,
                        Volatility = 0.05)
    
    sim_glick <- glicko2(games, status = initstate, history = TRUE, 
                         gamma = gamma)
    ## extract the rating of focal player and store it
    focal_glick <- sim_glick$history[focal_init$Player, ,]
    
    glicko_sim[i, j] <- as.numeric(focal_glick)[1]
    
  }
  
}

tru_glicko <- true_elo |> 
  rename(game = index, glicko = focal_elo)

## quick plot to confirm

quick_sim <- glicko_sim[, 1:num_sims]  

colnames(quick_sim) <- paste0("Sim_", 1:num_sims)


quick_sim <- as_tibble(quick_sim) |> 
  mutate(game = row_number()) |> 
  pivot_longer(cols = Sim_1:Sim_100, names_to = "Sim",
               values_to = "glicko") 

quick_sim |> 
  ggplot(aes(game, glicko)) +
  geom_line(alpha = 0.05, aes(group = Sim)) +
  geom_line(data = tru_glicko,
            col = "red")



# More complete plot for poster -------------------------------------------


rest_games <- small_data |> 
  filter(Username == focal_init$Player) |> 
  arrange(UTCDate, UTCTime) |> 
  slice(1:(n() - 1000)) |> 
  mutate(game = row_number())


num_prev <- nrow(rest_games)


sim_plot_data <- quick_sim |> 
  mutate(game = game + num_prev)

plot_true <- tru_glicko |> 
  mutate(game = game + num_prev)

#getting mean elo across each game
sim_mean_elo = sim_plot_data %>% group_by(game) %>% summarise(mean_elo = mean(glicko))


#theme_set(theme_1col)

ppd_elo_plot = rest_games |> 
  mutate(glicko = ifelse(Username == White, WhiteElo, BlackElo),
         game = row_number()) |>
  dplyr::select(game, glicko) |> 
  ggplot(aes(game, glicko, UTCDate, UTCTime)) +
  geom_line(linewidth = 0.5, col = "darkred") + 
  geom_line(data = sim_plot_data, mapping = aes(group = Sim), alpha = 0.05) +
  geom_line(data = sim_mean_elo, mapping = aes(x = game, y = mean_elo), alpha = 0.7) +
  geom_line(data = plot_true,
            col = "red", linewidth = 0.5) +
  xlim(c(28000, num_prev + 1000)) +
  xlim(c(85000, num_prev + 1000)) +
  ylim(c(2600, 3600)) +
  xlim(c(num_prev - 1000, num_prev + 1000)) +
  labs(x = "Games Played", y = "ELO")

ppd_elo_plot


ggsave(plot = ppd_elo_plot, 
       filename ="C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/ppc_elo_magnus_mean.png", dpi = 1000,
       height = 6, width = 8)



# Can also plot the number of Games Won -----------------------------------

## compare this to the truth for the single player


quick_sim_games <- game_sim[, 1:num_sims]  

colnames(quick_sim_games) <- paste0("Sim_", 1:num_sims)


quick_sim_games <- as_tibble(quick_sim_games) |> 
  mutate(game = row_number()) |> 
  pivot_longer(cols = Sim_1:Sim_100, names_to = "Sim",
               values_to = "Result") 

true_games_won <- sum(full_game_info$focal_result)

quick_sim_games |> 
  group_by(Sim) |> 
  summarise(games_won = sum(Result)) |> 
  ggplot(aes(games_won)) +
  geom_histogram() +
  geom_vline(aes(xintercept = true_games_won), col = "red")







#### RIDGE PLOT FOR PAPER ####


player_id <- as_tibble(gm_players) |> 
  mutate(id = row_number()) |> 
  rename(player_name = value)

player_pars <- gm_model$draws() %>% 
  as_draws_df() %>%
  dplyr::select(starts_with("beta[")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param)) |> 
  left_join(player_id, by = "id")


#Betas 

player_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  # geom_boxplot() +
  geom_density_ridges(scale = 1, rel_min_height = 0.1) +
  # coord_flip() +
  labs(y = "", x = "Estimated W/L-Effect") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  xlim(c(-0.25, 0.25)) +
  NULL

#boxplots
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  
}

player_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_summary(fun.data = quantiles_95, geom="density_ridges") + 
  #geom_boxplot(outlier.shape = NA, coef = 1.5) +
  #geom_density_ridges(scale = 1, rel_min_height = 0.1) +
  # coord_flip() +
  labs(y = "", x = TeX("95% Credible Interval of Estimated Experiential Effect ($\\beta_j$)")) +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed", size = 0.8) + 
  xlim(c(-0.3, 0.3)) +
  NULL


#alphas
player_pars |> 
  filter(param == 1) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  #geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.data = quantiles_95, geom="boxplot") + 
  #geom_density_ridges(scale = 1, rel_min_height = 0.1) +
  # coord_flip() +
  labs(y = "", x = TeX("95% Credible Interval of Estimated Player Effect ($\\alpha_j$)")) +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed", size = 0.8) + 
  xlim(c(-0.32, 0.32)) +
  NULL


#pay attention to edge cases, why is magnus's winner effect negative, I don't think this makes sense









#### AMATEUR VS GM FIXED EFFECTS IN BULLET #####
library(ggdist)
library(tidyverse)
library(viridis)



theme_3col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(1, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 1.5),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 4, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}
## compare global effects first
gm_pars <- gm_model$draws(c("mu_beta",
                            "gamma1", "gamma2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  mutate(model = "GM")

amateur_pars <- amateur_model$draws(c("mu_beta",
                                      "gamma1", "gamma2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  mutate(model = "1700-1900")

pars_20_22 = model_20_22$draws(c("mu_beta",
                              "gamma1", "gamma2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  mutate(model = "2000-2200")

pars_23_25 = model_23_25$draws(c("mu_beta",
                                 "gamma1", "gamma2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  mutate(model = "2300-2500")

pars_bot = bot_fit$draws(c("mu_beta",
                          "gamma1", "gamma2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"gamma2") |> 
  mutate(model = "Bots (~3600)")



# facet_labels <- as_labeller(c(
#   mu_beta = "mu[beta]",
#   gamma1 = "'Colour Effect ('*gamma[1]*')'",
#   gamma2 = "'Colour Effect ('*gamma[2]*')'",
#   sigma_1 = "sigma[1]",
#   `tau[1]` = "tau[1]",
#   `tau[2]` = "tau[2]",
#   sigma_g1 = "sigma[g[1]]",
#   sigma_g2 = "sigma[g[2]]"
# ), label_parsed)

facet_labels <- as_labeller(c(
  mu_beta = "mu[beta]",
  gamma1 = "gamma[1]",
  gamma2 = "gamma[2]",
  sigma_1 = "sigma[1]",
  `tau[1]` = "tau[1]",
  `tau[2]` = "tau[2]",
  sigma_g1 = "sigma[g[1]]",
  sigma_g2 = "sigma[g[2]]"
), label_parsed)

gm_amateur_comp_global <- bind_rows(gm_pars, amateur_pars, pars_20_22, pars_23_25) |> 
  ggplot(aes(x = value, y = name, color = factor(model, levels = c("1700-1900", "2000-2200", "2300-2500", "GM", "Bots (~3600)")))) +
  stat_pointinterval(position = position_dodge(width = 0.2)) +
  facet_wrap(~name,
             scales = "free",
             labeller = labeller(name = facet_labels)) +
  labs(x = element_blank(), y = element_blank(), color = element_blank()) +
  theme_3col() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = breaks_pretty(n = 2)) +
  scale_color_viridis_d(option = "H")

gm_amateur_comp_global


# ggsave(plot = gm_amateur_comp_global,
#        filename = "C:/Users/adamg/Desktop/Thesis/results/bot figures/fixed_global_comp_fit3.png", dpi = 1000,
#        height = 4, width = 8)


# getting point estimates

point_estimates =  bind_rows(gm_pars, amateur_pars) %>%
  group_by(name, model) %>%
  summarise(point_est = mean(value))
point_estimates



# 
# bullet_users_gm = readRDS(file = "C:/Users/adamg/Desktop/Thesis Fall 2024/CASSIS Poster/code stuff for getting figures/users_bullet_gm.RDS")
# 
# gm_model$draws(c("mu_beta",
#                  "gamma1", "gamma2"))








global_gm_bullet_prev <- model_23_25$draws(c("mu_beta",
                                                    "gamma1", "gamma2",
                                                    "sigma_1", "tau[1]", "tau[2]",
                                                    "sigma_g1", "sigma_g2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = "mu_beta":"sigma_g2") |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 4, scales = "free",
             labeller = labeller(name = facet_labels)) +
  labs(x = element_blank(), y = element_blank()) +
  theme_3col() +
  scale_x_continuous(breaks = breaks_pretty(n = 2)) 

global_gm_bullet_prev















### winner plots ###

axis_title <- 16
title_size <- 18
axis_text_size <- 14
legend_text <- 14
line_size <- 1
theme_single_y <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(1, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 2),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 5, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      legend.position = "none"
    )
}


#1700-1900
amateur_model = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/all_rated_prev_n1/all_rated_bullet_model_prev_n1_17_19.RDS")
amateur_users = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_17_19.RDS")

gm_model = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/all_rated_prev_n1/all_rated_bullet_model_prev_n1_gm.RDS")
gm_users = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_gm.RDS")

#2000-2200
model_20_22 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/all_rated_prev_n1/all_rated_bullet_model_prev_n1_20_22.RDS")
users_20_22 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_20_22.RDS")

#2300-2500
model_23_25 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/all_rated_prev_n1/all_rated_bullet_model_prev_n1_23_25.RDS")
users_23_25 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_23_25.RDS")



#QUICKLY CHECKING NEW POSTERIORS OF MIDDLE FITS WITH CORRECT STAN FILE
# 
# #1700-1900
# amateur_model = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/middle_fits_for_PPD_NEW/17_19_middle_1000_game.RDS")
# amateur_users = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_17_19.RDS")
# 
# gm_model = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/middle_fits_for_PPD_NEW/gm_middle_1000_game.RDS")
# gm_users = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_gm.RDS")
# 
# #2000-2200
# model_20_22 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/middle_fits_for_PPD_NEW/20_22_middle_1000_game.RDS")
# users_20_22 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_20_22.RDS")
# 
# #2300-2500
# model_23_25 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/middle_fits_for_PPD_NEW/23_25_middle_1000_game.RDS")
# users_23_25 = readRDS("C:/Users/adamg/Desktop/Thesis/results/model fits/users/users_bullet_23_25.RDS")


#gm_single_session_model = readRDS("C:/Users/adamg/Desktop/Thesis Fall 2024/models/single_session_fits/all_rated_bullet_single_session_prev_n3_gm.RDS")

#gm_model = readRDS("C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/GM_middle_1000_game_gen.RDS")


amateur_pars = amateur_model$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))

#assign player names to id
amateur_pars$player_name = amateur_users[amateur_pars$id]

winner_plot_amateur <- amateur_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "1700-1900 Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_amateur

winner_plot_amateur_win_percentage <- amateur_pars |> 
  filter(param == 2) |> 
  mutate(value = (invlogit(value) - 0.5)) %>% #take inv logit of the drawed beta_js, this is increased win percentage coming from a win vs coming from a loss - holding other factors constant 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Change in Win Probability", y = element_blank(), 
       title = "1700-1900 Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_amateur_win_percentage


# ggsave(plot = winner_plot_amateur, 
#        filename = "C:/Users/adamg/Desktop/joint seminar - Fall 2024/figures/momentum_amateurs.png", dpi = 1000,
#        height = 7, width = 6)

ggsave(plot = winner_plot_amateur, 
       filename = "C:/Users/adamg/Desktop/Thesis Fall 2024/code for paper figures/amateur_prev_winner.png", dpi = 1000,
       height = 7, width = 6)



#### GMS ####


gm_pars = gm_model$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))

#assign player names to id
gm_pars$player_name = gm_users[gm_pars$id]

winner_plot_gm <- gm_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "GM Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_gm

winner_plot_gm_win_percentage <- gm_pars |> 
  filter(param == 2) |> 
  mutate(value = (invlogit(value) - 0.5)) %>% #take inv logit of the drawed beta_js, this is increased win percentage coming from a win vs coming from a loss - holding other factors constant 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Change in Win Probability", y = element_blank(), 
       title = "GM Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_gm_win_percentage


# ggsave(plot = winner_plot_gm, 
#        filename = "C:/Users/adamg/Desktop/joint seminar - Fall 2024/figures/momentum_gms.png", dpi = 1000,
#        height = 7, width = 6)

ggsave(plot = winner_plot_gm, 
       filename = "C:/Users/adamg/Desktop/Thesis Fall 2024/code for paper figures/gm_prev_winner.png", dpi = 1000,
       height = 7, width = 6)






### 2000 - 2200 ###

#generate both change in win prob and also original effect scale

amateur_20_22_pars = model_20_22$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))

#assign player names to id
amateur_20_22_pars$player_name = users_20_22[amateur_20_22_pars$id]

winner_plot_amateur_20_22 <- amateur_20_22_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "2000-2200 Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_amateur_20_22

winner_plot_amateur_20_22_win_percentage <- amateur_20_22_pars |> 
  filter(param == 2) |> 
  mutate(value = (invlogit(value) - 0.5)) %>% #take inv logit of the drawed beta_js, this is increased win percentage coming from a win vs coming from a loss - holding other factors constant 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Change in Win Probability", y = element_blank(), 
       title = "2000-2200 Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_amateur_20_22_win_percentage


ggsave(plot = winner_plot_amateur_20_22_win_percentage, 
       filename = "C:/Users/adamg/Desktop/Thesis Fall 2024/code for paper figures/20_22_prev_winner_perc.png", dpi = 1000,
       height = 7, width = 6)





### 2300 - 2500 ###

#generate both change in win prob and also original effect scale

amateur_23_25_pars = model_23_25$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))

#assign player names to id
amateur_23_25_pars$player_name = users_23_25[amateur_23_25_pars$id]

winner_plot_amateur_23_25 <- amateur_23_25_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "2300-2500 Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_amateur_23_25

winner_plot_amateur_23_25_win_percentage <- amateur_23_25_pars |> 
  filter(param == 2) |> 
  mutate(value = (invlogit(value) - 0.5)) %>% #take inv logit of the drawed beta_js, this is increased win percentage coming from a win vs coming from a loss - holding other factors constant 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Change in Win Probability", y = element_blank(), 
       title = "2300-2500 Cohort") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_amateur_23_25_win_percentage


ggsave(plot = winner_plot_amateur_23_25, 
       filename = "C:/Users/adamg/Desktop/Thesis Fall 2024/code for paper figures/23_25_prev_winner.png", dpi = 1000,
       height = 7, width = 6)






