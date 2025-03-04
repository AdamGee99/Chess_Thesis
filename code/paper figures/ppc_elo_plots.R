library(tidyverse)
library(here)
library(MASS)
library(PlayerRatings)
library(posterior)


### FUNCTIONS ###

#function to read in separate files and merge
read_player = function(path, file){
  dat <- read_csv(file = paste0(data_path, "/", file),
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
      #filter(White == user | Black == user) %>% 
      filter(Username == user) |> #need to filter for username, since if we do white or black can add extra games since username can be the focal player or opponent
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
      #filter(White == user | Black == user) %>% 
      filter(Username == user) |>
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




### IMPORTING MODELS & DATA ###

#model fit on final 1000-2000 games
#some GMs don't have over 1000 games, so the results are probably messed up for them
model_path = here("results", "model fits", "middle_fits_for_PPD_NEW", "23_25_middle_1000_game.RDS")
model = readRDS(model_path)

#model = readRDS("C:/Users/adamg/Desktop/Thesis Fall 2024/models/single_session_fits/all_rated_bullet_single_session_prev_n5_gm.RDS")

users = readRDS(file = here("results", "model fits", "users", "users_bullet_23_25.RDS"))
data_path = here("data", "lichess2300-2500")
files = list.files(data_path)
lichess_data = files %>%
  map_dfr(~read_player(data_path, .x))



### TIDYING ###

#selecting bullet 60+0 only
bullet_60 = lichess_data %>% 
  filter(Event == "Rated Bullet game") %>% 
  filter(TimeControl == "60+0") %>%
  dplyr::select(c("White", "Black", "Result", "UTCDate", "UTCTime", "Username", "WhiteElo", "BlackElo"))

#remove the duplicate rows
bullet_60_no_dup = bullet_60[!duplicated(bullet_60),]

#final 1000 games each user plays
last_games = bullet_60_no_dup %>%
  filter(Username %in% users) %>%  
  group_by(Username) %>%
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>%
  slice_tail(n = 1000) %>%
  ungroup()

#only consider previous game for history
tidy_games = map_dfr(users, get_hist, last_games, prev_n = 1) |> 
  as_tibble()

#adding performance history

#selecting player to plot
#magnus_id = which(users == "DrNykterstein") #magnus
#nihal_id = which(users == "nihalsarin2004") #nihal

focal_player_used = which(users == "Lobovanvliet")
  
#slightly less than 1000 games since filtered out draws
focal_init_data = init_data |>
  filter(focal_user == users[focal_player_used])

focal_init = data.frame("Player" = users[focal_player_used],
                        "Rating" = ifelse(focal_init_data$White[1] == users[focal_player_used],
                                          focal_init_data$WhiteElo[1], focal_init_data$BlackElo[1]))
# slow for even 100 sims at the moment, due to the for loop
num_sims = 100
num_games = nrow(focal_init_data)

#simulations of the elo on the final 1000 games from the model
  #filter to only include the draws for however many sims you want to do, doesn't seem to speed it up much
focal_draws = model$draws() |> as_draws_df() |>
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

## the covariates for the future games
future_games_cov = tibble(focal = as.character(focal_init_data$focal_id),  
                          color = focal_init_data$focal_white,
                          elo_diff = focal_init_data$elo_diff,
                          hist = focal_init_data$ave_prop) 

#storage
glicko_sim <- matrix(NA, nrow = num_games, ncol = num_sims)
game_sim <- matrix(NA, nrow = num_games, ncol = num_sims)
probs = matrix(NA, nrow = num_games, ncol = num_sims) #for debugging

set.seed(2024)
#get the simulations
#should do this in parallel, all the sims are independent of eachother 
for (i in 1:num_games) {
  
  ## get the game information for the ith game
  game_info = future_games_cov[i, ]
  # gamma the colour effect for glicko calc
  #gamma = ifelse(game_info$color == 1, 1, -1)
  gamma = game_info$colour
  
  # get opponent name and ability
  opp_elo = focal_init_data[i,] |> 
    mutate(opp = ifelse(focal_white == 1, BlackElo, WhiteElo)) |> 
    pull(opp)
  opp = focal_init_data[i,] |> 
    mutate(opp_name = ifelse(focal_white == 1, Black, White)) |> 
    pull(opp_name)
  
  for (j in 1:num_sims) {
    curr_draw = focal_draws %>% filter(draw == j)
    
    ## if i = 1 use the initial glicko, otherwise use the previous
    if ( i == 1) {
      #curr_glick <- focal_init$Rating
      curr_glick = ifelse(focal_init_data$focal_white[1] == 1, focal_init_data$WhiteElo[1], focal_init_data$BlackElo[1])
      curr_prop <- 0 #assume they're current past performance is at their usual level
    } else {
      curr_glick <- glicko_sim[i-1, j]
      curr_prop <- game_sim[i-1, j] - mean(focal_init_data$focal_result) ## take lagged win/loss - mean win rate
    }
    ## then simulate the outcome and update the glicko
    inv_logit <- curr_draw$beta1 + curr_draw$beta2 * curr_prop +
      curr_draw$gamma1 * game_info$color +
      curr_draw$gamma2 * (curr_glick - opp_elo)
    prob <- exp(inv_logit)/(1 + exp(inv_logit))
    
    ## get sim result
    sim_result <- sample(c(1, 0), size = 1, prob = c(prob, 1 - prob))
    game_sim[i, j] <- sim_result #store wins/losses
    
    #find rating
    games <- data.frame(Week = i,
                        Player1 = focal_init$Player,
                        Player2 = opp,
                        Score = sim_result)
    initstate <- tibble(Player = c(focal_init$Player, opp),
                        Rating = c(curr_glick, opp_elo),
                        Deviation = 50,
                        Volatility = 0.04)
    
    sim_glick <- glicko2(games, status = initstate, history = TRUE, 
                         gamma = gamma)
    ## extract the rating of focal player and store it
    focal_glick <- sim_glick$history[focal_init$Player, ,]
    
    glicko_sim[i, j] <- as.numeric(focal_glick)[1]
  }
  print(i)
}
#save(game_sim, glicko_sim, file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/game_glicko_100_sim_est_elo_nihal.RData")
#load("C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/game_glicko_100_sim_true_elo_nihal.RData")


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
sim_results = foreach(sim = 1:num_sims, .combine = "cbind", .packages = c("tidyverse", "doParallel", "rstanarm", "PlayerRatings")) %dopar% {
  #the drawn parameter values to use for this simulation
  sim_draw = focal_draws %>% filter(draw == sim)
  
  #loop over games (no parallel)
  foreach(game = 1:num_games, .combine = "c") %do% {
    
    #indicator for colour effect, 1 is white, 0 is black
    colour_ind = future_games_cov[game,]$color
    
    #elo diff for rating effect
    rating_diff = future_games_cov[game,]$elo_diff
    
    ## if first game, use default history (0), otherwise use the previous
    if (i == 1) {
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
sim_results

#now get glicko2 rtaing for each simulation of games (the columns in sim_results)

#first do it in a for loop then parallelize

#the information for glicko in the final 1000 games, the only thing that changes between sims is the result of the games 

# #focal player and opponnent on the final 1000 games
# focal_player = focal_init$Player
# opponent = ifelse(focal_init_data$focal_white == 1, focal_init_data$Black, focal_init_data$White)
# opponent_rating = ifelse(focal_init_data$focal_white == 1, focal_init_data$BlackElo, focal_init_data$WhiteElo)
# 
# #this info is static - add score in the loop (changes between sims)
# glicko_info = data.frame(Week = 1:num_games,
#                          Player1 = focal_player,
#                          Player2 = opponent)
# #try using only intial rating here and updating it in the for loop
# initstate = tibble(Player = c(focal_player, opponent),
#                    Rating = c(focal_init$Rating, opponent_rating), 
#                    Deviation = 50,
#                    Volatiliy = 0.04) 
# 
# #see if you can figure out how to use date/time to get the players volatility/deviation - or if this is available in the raw data
# sim_glicko = matrix(NA, nrow = num_games, ncol = num_sims)
# for (i in 1:num_sims) {
#   curr_glicko_info = glicko_info %>% mutate(Score = unname(sim_results[,i]))
#   
#   for (j in 1:num_games) {
#     #the current status of the system
#     curr_focal_rating = ifelse(j == 1, focal_init$Rating, sim_glicko[j - 1, i])
#     curr_opp_rating = ifelse(focal_init_data[j,]$focal_white == 1, focal_init_data[j,]$BlackElo, focal_init_data[j,]$WhiteElo)
#     
#     curr_status = tibble(Player = c(focal_player, opponent[j]),
#                          Rating = c(ifelse(j == 1, focal_init$Rating, sim_glicko[j - 1, i]), opponent_rating[j]),
#                          Deviation = c(ifelse(j == 1, 50, curr_sim_rating$ratings$Deviation[1]), 50), #use the deviation and volatiliy from previous game
#                          Volatility = c(ifelse(j == 1, 0.04, curr_sim_rating$ratings$Volatility[1]), 0.04))
#     
#     # curr_status = tibble(Player = c(focal_player, opponent[j]),
#     #                      Rating = c(curr_focal_rating, curr_opp_rating), 
#     #                      Deviation = 50,
#     #                      Volatility = 0.04)
#     
#     #sim rating
#     curr_sim_rating = glicko2(curr_glicko_info[j,],
#                               status = curr_status,
#                               history = FALSE,
#                               gamma = ifelse(focal_init_data[j,]$focal_white == 1, 1, -1))
#     
#     #store it
#     sim_glicko[j, i] = curr_sim_rating$ratings$Rating[1]
#   }
#   print(i)
# }
# 
# glicko_sim = sim_glicko

#so this is the slow part
#should try to parallelize it


#true glicko rating of all games before last 1000
true_glicko_first = bullet_60_no_dup |> 
  filter(Username == focal_init$Player) |> 
  arrange(UTCDate, UTCTime) |> 
  slice(1:(n() - 1000)) |> 
  mutate(game = row_number(), glicko = ifelse(Username == White, WhiteElo, BlackElo)) %>%
  dplyr::select(game, glicko)
num_prev = nrow(true_glicko_first) #number of games before last 1000

#true glicko rating of final 1000 games
true_glicko_final = focal_init_data %>%
  mutate(game = row_number() + num_prev, glicko = ifelse(focal_white == 1, WhiteElo, BlackElo)) %>%
  dplyr::select(game, glicko)

#glicko rating across all sims on final 100 games
final_sims = data.frame(glicko_sim, check.names = FALSE) %>%
  mutate(game = row_number() + num_prev) %>%
  pivot_longer(cols = -game, names_to = "Sim", values_to = "glicko")

#mean glicko rating for each game across all sims
sim_mean_glicko = final_sims %>% group_by(game) %>% summarise(mean_glicko = mean(glicko))

#ploting
axis_title <- 16
title_size <- 18
axis_text_size <- 14
legend_text <- 14
line_size <- 1
theme_single <- function(){ 
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
                                margin = margin(t = 1.5, r = 1.5, b = 2, l = 1.5)),
      axis.title.y = element_text(size = axis_title, vjust = 2, angle = 90),
      #axis.text.y = element_blank(),
      #axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}

ppd_elo_plot = ggplot(data = true_glicko_first, aes(x = game, y = glicko)) +
  geom_line(linewidth = 0.5, col = "darkred") + 
  geom_line(data = final_sims, mapping = aes(group = Sim), alpha = 0.05) +
  geom_line(data = sim_mean_glicko, mapping = aes(x = game, y = mean_glicko), 
            alpha = 0.7) +
  geom_line(data = true_glicko_final, col = "red", linewidth = 0.5) +
  ylim(c(min(glicko_sim) - 50, max(glicko_sim) + 50)) +
  xlim(c(num_prev - 1500, num_prev + num_games)) +
  xlab("Games Played") + ylab("Glicko2 Rating") +
  theme_single()

ppd_elo_plot

#save the magnus/nihal results 
#save.image(file = "nihal_elo_ppd_100sims.RData")

# ggsave(plot = ppd_elo_plot,
#        filename ="C:/Users/adamg/Desktop/Thesis/results/paper figures/Figure_S2_right.png", dpi = 1000,
#        height = 4, width = 6)

#17_19_1 is "APlayerfromEarth"
#17_19_2 is "d8336"
#17_19_3 is "larrywheels"

#20_22_1 is "Graminho"
#20_22_2 is "Rerereggie"

#23_25_1 is "NorthernBerserker"
#23_25_2 is "Lobovanvliet"


