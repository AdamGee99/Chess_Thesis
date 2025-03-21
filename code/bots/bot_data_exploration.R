#### JAN 15, 2025 ####

#Bot data exploration
#Exploring all TCEC games up to current date (season 27)

library(tidyverse)
library(data.table)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(ggridges)
library(scales)
library(ggdist)
library(rstanarm)
library(bigchess) #package for reading in PGNs

source(here("code", "helper functions", "helper.R"))
source(here("code", "helper functions", "plot_templates.R"))

get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games %>% 
      filter(Username == user) %>%
      arrange(Round) %>% 
      mutate(focal_white = ifelse(Username == White, 1, 0)) %>% 
      #dplyr::select(White:BlackElo, focal_white) %>% 
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


# #these are the classical games, i dont think this has any blitz/bullet
# tcec_classical = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/bot data/TCEC-everything/TCEC-everything-compet-traditional.pgn")
# 
# #the bonus and test games - contains bullet, blitz, rapid games
# tcec_bonus = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/bot data/TCEC-everything/TCEC-everything-bonus-test.pgn")
# 
# #problem is that for each "season" they only play a small amount of games in these formats 
# #we can combine games from multiple seasons that use the same format, but 
#   #the problem with this is that the bots playing in each season will be different - Im not sure this is a problem though
#   #another problem is that the same bot will be programmatically different between seasons - this is the real problem!!!! Its not technically the same bot
# 
# #getting games from CCC would be better since they play much more games in these shorter time formats
# 
# 
# 
# ccc_test = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/bot data/tournament-216562.pgn")
# 
# 
# 
# #need to figure out how to merge these but retaining the correct order - only gives the
# adam_games_white = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/data/chessguy1515-white.pgn")
# adam_games_black = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/data/chessguy1515-black.pgn")
# 
# 
# #43,536 games played in Dec 2024
# #from website CCRL - blitz 2+1 time control 
# dec_2024_games = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/bot data/2024-12.bare.[43536].pgn/2024-12.bare.[43536].pgn")
# 
# 
# num_games_engine = dec_2024_games %>% group_by(White) %>% summarise(num_games = n()) %>%
#   arrange(desc(num_games))
# num_games_engine
# #most played games by engines are 643
# 
# 
# #can also download games by engine - this will give way more games for each engine 
# #4083 blitz games from stockfish 17 - each engine has so many versions 
# stockfish_17 = read.pgn("C:/Users/adamg/Desktop/Thesis Fall 2024/bot data/Stockfish_17_64-bit.bare.[4083].pgn/Stockfish_17_64-bit.bare.[4083].pgn")


#downloaded 11 of the most popular/strongest engines 
data_path = here("data", "bot data", "CCRL", "blitz - engines")
files = list.files(data_path)

engine_blitz_raw = files %>% map_dfr(~read.pgn(paste0(data_path, "/", .x)))

#openings - use score of opening as additional covariate in model
  #its the draw rate and white SCORE! 
openings = read.csv(here("data", "bot data", "CCRL", "ccrl_openings.csv")) %>%
  mutate(across(c(Draws, White_win), ~ gsub(.x ,pattern = "%", replacement = ""))) %>%
  mutate(across(c(Draws, White_win), as.numeric)) %>%
  mutate(across(c(Draws, White_win), ~ .x/100)) %>%
  rename(white_score = White_win, draw_rate = Draws, num_games = Num_games) %>%
  mutate(black_score = 1 - white_score)


#the score needs to correspond to the side of the focal player

  
#cleaning the openings and adding win rate
engine_blitz = engine_blitz_raw %>% 
  mutate(Opening = ifelse(is.na(Variation), Opening,  paste0(Opening, ", ", Variation))) %>%
  dplyr::select(-Variation) %>%
  mutate(focal_opening_win_rate = openings$white_score[match(Opening, openings$Opening)]) %>%
  na.omit() %>%
  mutate(Round1 = sapply(strsplit(Round, "\\."), `[`, 1)) %>%  #Round is split into sub tournaments,...
  mutate(Round2 = sapply(strsplit(Round, "\\."), `[`, 2)) %>%
  mutate(Round3 = sapply(strsplit(Round, "\\."), `[`, 3)) %>%
  group_by(Username, Round1, Round2) %>%
  arrange(Username, Round1, Round2, .by_group = TRUE) %>%
  ungroup()

#I removed the games where the opening is not in the database
#this is small, only 42 games -- nrow(engine_blitz_raw) - nrow(engine_blitz)


#most popular openings
engine_blitz$Opening %>% table() %>% sort(decreasing = TRUE) %>% head()

#try fitting only on one opening
#engine_blitz = engine_blitz %>% filter(Opening == "English opening")


#try encoding it so a draw with black is a lose for white (win for black)
  #I think we should filter out the openings that have high black win rate though

engine_blitz$Result %>% table()
#almost all games draw, 14,000 wins, 6000 loses - not sure how to treat the loses

# engine_blitz = engine_blitz %>%
#   mutate(Result = ifelse(Result == "1-0", "1-0", "0-1"))


#the opening win rate parameter is wrong - there are openings with 0% win rate but the game is won by white against an equally rated engine


#only 2648 games played - split between 11 engines


#users means engine names
users = engine_blitz$Username %>% unique()


tidy_games <- map_dfr(users, get_hist, engine_blitz, prev_n = 1) %>% 
  as_tibble()


init_data <- tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  mutate(rating_diff = ifelse(focal_white == 1,
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  dplyr::select(focal_user, Round, focal_id, focal_white,
                focal_win_prop, rating_diff, focal_result, White, Black, WhiteElo, BlackElo, focal_opening_win_rate) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  filter(focal_result != 0.5)
  # mutate(ave_prop = ifelse(ave_prop < -0.25, -1, 
  #                          ifelse(ave_prop < 0.25, 0, 1))) #changing the history of a draw to be 0....
  #filter(abs(ave_prop) > 0.25) #removing using draws as history for testing... should play no role in determining winner effects, but may help with estimation of other effects 
#so maybe keep the draws as history  


#table of current result and previous result
init_data %>% filter(focal_user == users[1]) %>% 
  mutate(test = paste0("result: ", as.character(focal_result), ", prev result: ", as.character((round(ave_prop*2) + 1) /2))) %>% 
  pull(test) %>% table()


# the difference is when focal result is loss, much more likely to come from win in permuted compared to raw
# IN permuted... given prev game is a loss, prob of winning next game is roughly the same as regular win rate
# IN raw... given prev game is a loss, prob of winning next game is MUCH lower than regular win rate
# this is the difference

  #maybe this is due to rematching the same strong opponent over and over? - eg if playing stockfish 10 times in a row mostly likely will lose and keep losing 
  #could it be due to the opening?

#given the past result was a win 


stan_data_ave <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$rating_diff,
                      win_prop = init_data$ave_prop,
                      open_win_rate = init_data$focal_opening_win_rate)


#stan_file <- here("owen", "cluster_scripts", "final_model_scale_priors.stan")
stan_file <- here("code", "stan", "final_model_scale_priors_NEW.stan")
fit = "fit1_standard_hist"

mod <- cmdstan_model(stan_file)

bot_fit <- mod$sample(data = stan_data_ave,
                      seed = 123,
                      chains = 4,
                      parallel_chains = 4,
                      refresh = 100)

bot_fit$save_object(file = here("results", "model fits", "bot fits", paste0(fit, ".RDS")))

#fit 1 is same model used for the humans
#fit 2 is model with gamma3
#fit 3 is model with gamma3 and no intercept

#fit1_single_opening (single_opening_1) is queens pawn opening (most played)
#fit1_single_opening (single_opening_2) is reti opening (second most played)
#fit1_single_opening (single_opening_3) is English opening (third most played)

#bot_fit = readRDS(here("results", "model fits", "bot fits", "fit1.RDS"))


### PLOTTING ### 



### global pars ###

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


facet_labels <- as_labeller(c(
  mu_beta = "mu[beta]",
  gamma1 = "gamma[1]",
  gamma2 = "gamma[2]",
  gamma3 = "gamma[3]",
  sigma_1 = "sigma[1]",
  `tau[1]` = "tau[1]",
  `tau[2]` = "tau[2]",
  sigma_g1 = "sigma[g[1]]",
  sigma_g2 = "sigma[g[2]]",
  sigma_g3 = "sigma[g[3]]"
), label_parsed)

# #for fit 3 (no alpha)
# facet_labels <- as_labeller(c(
#   mu_beta = "mu[beta]",
#   gamma1 = "gamma[1]",
#   gamma2 = "gamma[2]",
#   gamma3 = "gamma[3]",
#   sigma_1 = "sigma[1]",
#   tau = "tau",
#   sigma_g1 = "sigma[g[1]]",
#   sigma_g2 = "sigma[g[2]]",
#   sigma_g3 = "sigma[g[3]]"
# ), label_parsed)



global_gm_bullet_prev <- bot_fit$draws(c("mu_beta",
                                                         "gamma1", "gamma2",
                                                         "sigma_1", "tau",
                                                         "sigma_g1", "sigma_g2")) |> 
  as_draws_df() |> 
  pivot_longer(cols = -c(.chain, .iteration, .draw)) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 5, scales = "free",
             labeller = labeller(name = facet_labels)) +
  labs(x = element_blank(), y = element_blank()) +
  theme_single_y() +
  scale_x_continuous(breaks = breaks_pretty(n = 2)) 

global_gm_bullet_prev

ggsave(plot = global_gm_bullet_prev,
       filename = here("results", "bot figures", fit, "global_fixed.png"), dpi = 1000,
       height = 4, width = 8)








#### winner effects ####

# #for fit3
# bot_pars = bot_fit$draws() |>
#   as_draws_df() |>
#   dplyr::select(starts_with("beta")) |>
#   pivot_longer(cols = everything()) |>
#   mutate(id = stringr::str_extract(name, pattern = "\\d+]"),
#          id = stringr::str_replace(id, "\\]", "")) |>
#   mutate(id = as.numeric(id))
# 
# #assign player names to id
# bot_pars$player_name = users[bot_pars$id]
# 
# winner_plot_bot <- bot_pars |>
#   ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
#   stat_histinterval() +
#   labs(x = "Estimated Experiential Effect", y = element_blank(),
#        title = "Bots") +
#   geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
#   theme_single_y()
# winner_plot_bot


#for fit 1,2 (with alpha)
bot_pars = bot_fit$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))
bot_pars$player_name = users[bot_pars$id]

winner_plot_bot <- bot_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_bot

ggsave(plot = winner_plot_bot,
       filename = here("results", "bot figures", fit, "winner.png"), dpi = 1000,
       height = 6, width = 7)



# winner_plot_bot_win_perc <- bot_pars |> 
#   filter(param == 2) |> 
#   mutate(value = (invlogit(value) - 0.5)) %>% #take inv logit of the drawed beta_js, this is increased win percentage coming from a win vs coming from a loss - holding other factors constant 
#   ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
#   stat_histinterval() +
#   labs(x = "Change in Win Probability", y = element_blank(), 
#        title = "Bots") +
#   geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
#   theme_single_y()
# winner_plot_bot_win_perc
# 
# # ggsave(plot = winner_plot_bot,
# #        filename = "C:/Users/adamg/Desktop/Thesis/results/bot figures/winner_fit4.png", dpi = 1000,
# #        height = 6, width = 7)


int_plot_bot <- bot_pars |> 
  filter(param == 1) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Player Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
int_plot_bot

ggsave(plot = int_plot_bot,
       filename = here("results", "bot figures", fit,  "int.png"), dpi = 1000,
       height = 6, width = 7)


#clearly just correlated with rating
#this still confuses me... isn't this basically just an effect accounting for rating?


#the player effect and rating effect are correlated!!! this should be added to the model 
  #this is why the player effect is extremely negative and gamma3 extremely positive

#should opening_win_score be offset? Do we know the true value of the parameter?



# ggsave(plot = int_plot_bot,
#        filename = "C:/Users/adamg/Desktop/Thesis/results/bot figures/winner_perc.png", dpi = 1000,
#        height = 6, width = 7)




### PPDS ###


#counting wins but do it locally



#just start by doing it for one bot
#then do it for all bots
#then parallelize it!!





focal_player_used = 1

num_sims = 500
num_games = min(500, init_data %>% filter(focal_user == users[focal_player_used]) %>% nrow())

#last 500 games for focal user
focal_init_data = init_data |>
  filter(focal_user == users[focal_player_used]) %>%
  slice_tail(n = num_games)

#the true number of wins on the final 500 games
true_num_wins = focal_init_data$focal_result %>% sum()
  

focal_init = data.frame("Player" = users[focal_player_used],
                        "Rating" = ifelse(focal_init_data$White[1] == users[focal_player_used],
                                          focal_init_data$WhiteElo[1], focal_init_data$BlackElo[1]))


#simulations of the elo on the final 1000 games from the model
#filter to only include the draws for however many sims you want to do, doesn't seem to speed it up much
focal_draws = bot_fit$draws() |> as_draws_df() |>
  dplyr::select(starts_with(c("beta[", "gamma"))) |>
  mutate(draw = row_number()) |>
  pivot_longer(cols = "beta[1,1]":paste0("beta[2,", as.character(length(users)), "]")) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |>
  dplyr::select(-name) |>
  pivot_wider(names_from = param, values_from = value) |>
  rename(beta1 = `1`, beta2 = `2`) %>%
  filter(id == focal_player_used) %>%
  filter(draw %in% 1:num_sims)

# #for fit3 (no alpha)
# focal_draws = bot_fit$draws() |> as_draws_df() |>
#   dplyr::select(starts_with(c("beta", "gamma"))) |>
#   mutate(draw = row_number()) |>
#   pivot_longer(cols = "beta[1]":paste0("beta[",as.character(length(users)), "]")) |>
#   mutate(id = stringr::str_extract(name, pattern = "\\d+]"),
#          id = stringr::str_replace(id, "\\]", "")) |>
#   dplyr::select(-name) |>
#   rename(beta = value) %>%
#   filter(id == focal_player_used) %>%
#   filter(draw %in% 1:num_sims)

## the covariates for the future games
future_games_cov = tibble(focal = as.character(focal_init_data$focal_id),  
                          color = focal_init_data$focal_white,
                          rating_diff = focal_init_data$rating_diff,
                          open_white_avg_score = focal_init_data$focal_opening_win_rate,
                          hist = focal_init_data$ave_prop) 



#simulate the number of wins in the final 500 games, then repeat this 100 times to make a histogram (poster predictive distribution)
game_sim = matrix(NA, nrow = num_games, ncol = num_sims)


#running in parallel
library(foreach)
library(doParallel)

n_cores = 10
cluster = parallel::makeCluster(
  n_cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = cluster)

set.seed(2025)
#parallelize the sim loop, the sims are independent but the rating of the games in each sim are not
sim_results = foreach(sim = 1:num_sims, .combine = "cbind", .packages = c("tidyverse", "doParallel", "rstanarm")) %dopar% {
  #the drawn parameter values to use for this simulation
  sim_draw = focal_draws %>% filter(draw == sim)
  
  #loop over games (no parallel)
  foreach(game = 1:num_games, .combine = "c") %do% {
    
    #indicator for colour effect, 1 is white, 0 is black
    colour_ind = future_games_cov[game,]$color
    
    #elo diff for rating effect
    rating_diff = future_games_cov[game,]$rating_diff
    
    #if first game, use default history (0), otherwise use the previous
    if (game == 1) {
      curr_hist = 0 #assume they're current past performance is at their usual level
    } else {
      curr_hist = future_games_cov[game,]$hist
    }
    
    #find probability
    lin_comb = sim_draw$beta1 + 
      (sim_draw$beta2 * curr_hist) +
      (sim_draw$gamma1 * colour_ind) +
      (sim_draw$gamma2 * rating_diff)
    prob = invlogit(lin_comb)
    
    #simulate result of each game
    sim_result = sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))
    
    sim_result
  } 
}
colnames(sim_results) = paste0("sim_", seq(1:num_sims))
rownames(sim_results) = paste0("game_", seq(1:num_games))
#sim_results

#convert to df
ppd = sim_results %>% colSums() %>% unname()
ppd_df = data.frame("num_wins" = ppd)

wins_ppd = ggplot(data = ppd_df, aes(x = num_wins)) +
            geom_histogram() +
            geom_vline(xintercept = true_num_wins, col = "red") +
            theme_bw()
wins_ppd

ggsave(plot = wins_ppd,
       filename = here("results", "bot figures", fit,  "ppd.png"), dpi = 1000,
       height = 6, width = 7)






#this is severely flawed due to removing all the draws - for eg, stockfish wins 480/500 games if we filter out draws
  #this is why the colour and rating effect are so large
  #doesnt explain the winner effects though 








### OLD CODE ###





# #loop through each game first, get the info for it (this way you don't need to get info every loop)
# for (i in 1:num_games) {
#   #current game info
#   curr_game_info = future_games_cov[i,]
# 
#   #indicator for colour effect, 1 is white, 0 is black
#   colour_ind = curr_game_info$color
# 
#   #elo diff for rating effect
#   rating_diff = curr_game_info$rating_diff
# 
#   #simulate wins/loss on current game
#   for (j in 1:num_sims) {
#     curr_draw = focal_draws %>% filter(draw == j)
# 
#   ## if first game use default history (0), otherwise use the previous
#   if (i == 1) {
#     curr_hist = 0 #assume they're current past performance is at their usual level
#   } else {
#     curr_hist = curr_game_info$hist
#   }
# 
#   #find probability
#   lin_comb = curr_draw$beta1 + (curr_draw$beta2 * curr_hist) +
#              (curr_draw$gamma1 * colour_ind) +
#              (curr_draw$gamma2 * rating_diff)
#   prob = invlogit(lin_comb)
# 
#   #simulate result
#   sim_result = sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))
# 
#   #store
#   game_sim[i, j] = sim_result
#   }
# }

#for fit3 (no alpha)
# for (i in 1:num_games) {
#   #current game info
#   curr_game_info = future_games_cov[i,]
# 
#   #indicator for colour effect, 1 is white, 0 is black
#   colour_ind = curr_game_info$color
# 
#   #elo diff for rating effect
#   rating_diff = curr_game_info$rating_diff
#   
#   #avg white score for current opening
#   open_white_avg_score = curr_game_info$open_white_avg_score
# 
#   #simulate wins/loss on current game
#   for (j in 1:num_sims) {
#     curr_draw = focal_draws %>% filter(draw == j)
# 
#     ## if first game use default history (0), otherwise use the previous
#     if (i == 1) {
#       curr_hist = 0 #assume they're current past performance is at their usual level
#     } else {
#       curr_hist = curr_game_info$hist
#     }
# 
#     #find probability
#     lin_comb = (curr_draw$beta * curr_hist) +
#       (curr_draw$gamma1 * colour_ind) +
#       (curr_draw$gamma2 * rating_diff) +
#       (curr_draw$gamma3 * open_white_avg_score)
#     prob = invlogit(lin_comb)
# 
#     #simulate result
#     sim_result = sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))
# 
#     #store
#     game_sim[i, j] = sim_result
#   }
# }


