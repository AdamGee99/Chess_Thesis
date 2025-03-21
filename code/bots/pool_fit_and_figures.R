#### MARCH 12, 2025 ####

#Creating file that fits model on random pool data (same data as we used in paper) and generates important plots

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


#where data is stored
cohort = "gm"
data_path = here("data", "lichessBots", cohort)
files = list.files(data_path)
lichess_data = files %>%
  map_dfr(~read_player(data_path, .x))

#bots that have big breaks between games
#bad_users = c("TheMatrix2029", "IA_Horus", "que-encrypt", "ElPeonElectrico")

# test = read.csv("C:/Users/adamg/Desktop/Thesis/data/lichessBots/lichess_bot_games_1.csv")
# test = test %>% filter(Username == "duchessAI")
# write.csv(test, file = here("data", "lichessBots", "gm", "duchessAI.csv"))

#filter for only 60+0 standard rated bullet games, arrange by Date-Time
#only filtering for rated bullet (not just 60+0) since they don't play 60+0 that much for some reason
small_data = lichess_data %>%
  filter(Event %in% c("Rated Bullet game", "Rated bullet game"), Variant == "Standard") %>%
  #filter(is.na(WhiteTitle) | is.na(BlackTitle)) %>%
  mutate(UTCDate = ymd(UTCDate), UTCTime = hms(UTCTime)) %>%
  group_by(Username) %>%
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>%
  #slice_tail(n = 500) %>%
  #slice(50:n() - 50) %>% #trim the first and last 50 games of each engine - their ratings aren't stable then, weird things going on...
  ungroup() #%>%
  #filter(!Username %in% bad_users)




# ### PERMUTE ###
# small_data = small_data[sample(nrow(small_data), size = nrow(small_data), replace = FALSE),] %>%
#   arrange(Username)

users = small_data$Username %>% unique() %>% sort()

#get history
tidy_games = map_dfr(users, get_hist, small_data, prev_n = 1) %>% 
  as_tibble()

#final tidying
init_data = tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  filter(!is.na(WhiteElo) & !is.na(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  mutate(elo_diff = ifelse(focal_white == 1,
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  mutate(opponent = ifelse(focal_white == 1, Black, White)) %>%
  mutate(opponent_id = match(opponent, users)) %>%
  dplyr::select(focal_user, focal_id, opponent_id, focal_white, White, Black, WhiteElo, BlackElo,
                focal_win_prop, elo_diff, focal_result) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  mutate(prev_result = lag(focal_win_prop, default = 0)) %>%
  filter(focal_result != 0.5) %>%
  ungroup() %>%
      ###filtering out rematches and big elo differences
  mutate(opp = ifelse(focal_white == 1, Black, White)) %>%
  filter(lag(opp) != opp) %>% #filter out rematches
  #filter(sign(elo_diff) != sign(lag(elo_diff))) %>%
  filter(abs(elo_diff) <= 200) #trying to mimic player games - they hardly ever play people rated 200 away from them

#good for checking if winner effects should exist
# init_data %>% filter(focal_user == users[9]) %>%
#   mutate(prev_result  = as.numeric((round(ave_prop*2) + 1) /2)) %>%
#   mutate(result = focal_result) %>%
#   select(result, prev_result) %>%
#   table()
    #this bot just rematches a stronger bot over and over, makes the winner effect huge

#probabilities given last result
prob_given_last_result = init_data %>%
  mutate(result = focal_result) %>%
  group_by(focal_user, prev_result) %>%
  summarise(win_prob = signif(mean(result), 2),
            n = n()) %>%
  ungroup()
prob_given_last_result

prob_given_last_result_plot = ggplot(data = prob_given_last_result, mapping = aes(x = prev_result, y = win_prob)) +
  geom_line() +
  facet_wrap(~focal_user)
prob_given_last_result_plot

#the mean elo diffs are nowhere close to 0.. this is a problem I think
init_data %>% group_by(focal_user) %>% summarise(mean_elo_diff = mean(elo_diff))


#plot rating over time
# init_data %>% filter(focal_id == 10) %>%
#   mutate(focal_elo = ifelse(focal_white == 1, WhiteElo, BlackElo)) %>%
#   pull(focal_elo) %>%
#   plot()


### FIT MODEL ###

stan_file = here("code", "stan", "final_model_scale_priors_NEW.stan")
fit = paste0(cohort, "_no_rematch_close_diff") #name of the current model fit

#list for stan
stan_data_ave <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$elo_diff,
                      win_prop = init_data$ave_prop)

#filter for closed system for opponent effect and add appropriate variables to stan list
if (endsWith(stan_file, "opponent_effect.stan")) {
  init_data = init_data %>% filter(!is.na(opponent_id))
  
  stan_data_ave$id_focal = init_data$focal_id
  stan_data_ave$id_opp = init_data$opponent_id
}

mod <- cmdstan_model(stan_file)

mod_fit <- mod$sample(data = stan_data_ave,
                      seed = 123,
                      chains = 4,
                      parallel_chains = 4,
                      refresh = 100)

#save fit
mod_fit$save_object(file = here("results", "model fits", "lichessBots", paste0(fit, ".RDS")))
saveRDS(users, file =  here("results", "model fits", "lichessBots", "users.RDS"))

#loading prev fit
#mod_fit = readRDS(file = here("results", "model fits", "lichessBots", "2000-2200.RDS"))



### PLOTTING ###

#save prob_given_last plot
ggsave(plot = prob_given_last_result_plot, 
       filename = here("results", "lichessBots", fit, "prob_given_last.png"), dpi = 1000,
       width = 7, height = 6)

facet_labels <- as_labeller(c(
  mu_beta = "mu[beta]",
  gamma = "gamma",
  gamma1 = "gamma[1]",
  gamma2 = "gamma[2]",
  gamma3 = "gamma[3]",
  sigma_1 = "sigma[1]",
  sigma_b = "sigma[beta]",
  sigma_a = "sigma[alpha]",
  sigma_g = "sigma[gamma]",
  `tau[1]` = "tau[1]",
  `tau[2]` = "tau[2]",
  sigma_g1 = "sigma[g[1]]",
  sigma_g2 = "sigma[g[2]]",
  sigma_g3 = "sigma[g[3]]"
), label_parsed)


vars = mod_fit$summary() %>% pull(variable)
vars = vars[!startsWith(vars, "beta")]
vars = vars[!startsWith(vars, "lp")]
vars = vars[!startsWith(vars, "L_Omega")]
vars = vars[!startsWith(vars, "nu")]
vars = vars[!startsWith(vars, "alpha")]
vars


global_bullet_prev <- mod_fit$draws(vars) |> 
  as_draws_df() |> 
  pivot_longer(cols = -c(.chain, .iteration, .draw)) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 5, scales = "free",
             labeller = labeller(name = facet_labels)) +
  labs(x = element_blank(), y = element_blank()) +
  theme_single_y() +
  scale_x_continuous(breaks = breaks_pretty(n = 2)) 
global_bullet_prev

#save
ggsave(plot = global_bullet_prev,
       filename = here("results", "lichessBots", fit, "global_fixed.png"), dpi = 1000,
       height = 4, width = 8)



#winner

#final_model_scale_priors_NEW.stan
mod_pars = mod_fit$draws() |> 
  as_draws_df() |>
  select(-any_of(c("lp__", ".chain", ".iteration", ".draw"))) |>
  pivot_longer(cols = everything()) |> 
  filter(startsWith(name, "beta[")) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))
mod_pars$player_name = users[mod_pars$id]

#opponent_effect.stan
# mod_pars = mod_fit$draws() |> 
#   as_draws_df() |>
#   select(-any_of(c("lp__", ".chain", ".iteration", ".draw"))) |>
#   pivot_longer(cols = everything()) |> 
#   mutate(param = case_when(
#     startsWith(name, "alpha") ~ 1,
#     startsWith(name, "beta") ~ 2,
#     .default = NA
#   )) %>%
#   filter(!is.na(param)) %>%
#   mutate(id = sub(".*\\[(\\d+)\\]", "\\1", name)) |>
#   mutate(id = as.numeric(id))
# mod_pars$player_name = users[mod_pars$id]


winner_plot_bot <- mod_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_bot


# player_level_winner_plot_bot <- mod_pars |> 
#   filter(param == 2) |> 
#   mutate(value = value - pull(mod_fit$summary("mu_beta"), mean)) %>%
#   ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
#   stat_histinterval() +
#   labs(x = "PLAYER LEVEL PROPORTION of Estimated Experiential Effect", y = element_blank(), 
#        title = "Bots") +
#   geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
#   theme_single_y()
# player_level_winner_plot_bot

#save
ggsave(plot = winner_plot_bot,
       filename = here("results", "lichessBots", fit, "winner.png"), dpi = 1000,
       height = 6, width = 7)


#player
int_plot_bot <- mod_pars |> 
  filter(param == 1) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Player Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
int_plot_bot

ggsave(plot = int_plot_bot,
       filename = here("results", "lichessBots", fit,  "int.png"), dpi = 1000,
       height = 6, width = 7)







### QUICK PPC CHECK ###





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
focal_draws = mod_fit$draws() |> as_draws_df() |>
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
                          rating_diff = focal_init_data$elo_diff,
                          #open_white_avg_score = focal_init_data$focal_opening_win_rate,
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
       filename = here("results", "lichessBots", fit,  "ppd.png"), dpi = 1000,
       height = 6, width = 7)






