#testing permutation


users = readRDS(file = here("results", "model fits", "users", "users_bullet_gm.RDS"))
data_path = here("data", "lichessGrandmasters")
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


#only fit on select users
num_users_to_fit = 20
select_users <- bullet_60_no_dup %>% 
  group_by(Username) %>% 
  tally() %>% 
  arrange(-n) %>% 
  ## will change this to n = 20 when run on cluster potentially
  slice_max(order = n, n = num_users_to_fit) %>% 
  pull(Username)




#just run on last 100 games for 10 users
last_games = bullet_60_no_dup %>%
  filter(Username %in% select_users) %>%  
  group_by(Username) %>%
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>%
  slice_tail(n = 1000)  %>% 
  mutate(Result = sample(Result)) %>% 
  ungroup()

tidy_games <- map_dfr(users, get_hist, last_games, prev_n = 1) %>% 
  as_tibble()

init_data <- tidy_games %>% 
  mutate(WhiteElo = as.numeric(WhiteElo), 
         BlackElo = as.numeric(BlackElo)) %>% 
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>% 
  mutate(elo_diff = ifelse(focal_white == 1, 
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>% 
  mutate(focal_id = match(focal_user, users)) %>% 
  select(focal_user, focal_id, focal_white, 
         focal_win_prop, elo_diff, focal_result) %>% 
  group_by(focal_id) %>% 
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_win_prop)) %>% 
  filter(focal_result != 0.5)



stan_data_ave_last <- list(N = nrow(init_data),
                           J = length(users),
                           y = init_data$focal_result,
                           id = init_data$focal_id,
                           colour = init_data$focal_white,
                           elo = init_data$elo_diff,
                           win_prop = init_data$ave_prop)




#stan_file <- here("owen", "cluster_scripts", "final_model_scale_priors.stan")
stan_file <- here("code", "stan", "final_model_scale_priors.stan")

mod <- cmdstan_model(stan_file)

perm_fit_test <- mod$sample(data = stan_data_ave_last,
                            seed = 123,
                            chains = 4,
                            parallel_chains = 4,
                            refresh = 100)




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



global_gm_bullet_prev <- perm_fit$draws(c("mu_beta",
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
       filename = here("results", "model checking", "simulated player", "global_fixed.png"), dpi = 1000,
       height = 4, width = 8)





#for fit 1,2 (with alpha)
perm_pars = perm_fit$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))
#perm_pars$player_name = users[perm_pars$id]
perm_pars$player_name = ifelse(perm_pars$id == 6, "SIMULATED PLAYER", users[perm_pars$id])

winner_plot <- perm_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot

ggsave(plot = winner_plot,
       filename = here("results", "model checking", "simulated player", "winner.png"), dpi = 1000,
       height = 6, width = 7)




int_plot_bot <- perm_pars |> 
  filter(param == 1) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estiamted Player Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
int_plot_bot

ggsave(plot = int_plot_bot,
       filename = here("results", "model checking", "simulated player", "int.png"), dpi = 1000,
       height = 6, width = 7)








#simulate a hypothetical player with baseline probability of 0.475 to win against an equally rated opponent as black
#also assume their rating is 2500 and they play players +- 200 of their rating
#assume their alpha is -0.05, gamma_1 is 0.2, gamma_2 is 0.005
#assume their rating is static (not sure if this is ok)
set.seed(2025)
num_games = 5000
sim_colour = rbinom(n = num_games, size = 1, prob = 0.5) #1 is white, 0 is black
sim_opp_rating = rnorm(n = num_games, mean = 2500, sd = 100)

sim_prob = invlogit(-0.05 + 0.2*sim_colour + 0.005*(2500 - sim_opp_rating)) #simulated probability of winning each game
sim_results = rbinom(n = num_games, size = 1, prob = sim_prob) #simulated results 
sim_hist = lag(sim_results, default = 0) - mean(sim_results) #simulated history

sim_rating = numeric()
sim_rating[1] = 2500
for(i in 2:num_games) {
  sim_rating[i] = ifelse(sim_results[i-1] == 1, sim_rating[i-1] + 1, sim_rating[i-1] - 1) #assume they gain +6 from win, -6 from loss
}
plot(sim_rating)

#these games are truly independent - so if we still see winner effects then something weird is going on with model




#add this simulated data to other real data (so we can still get real global parameters impact)
stan_data_ave$J = stan_data_ave$J + 1 #num players
stan_data_ave$y = c(stan_data_ave$y, sim_results)
stan_data_ave$N = length(stan_data_ave$y)
stan_data_ave$id = c(stan_data_ave$id, rep(6, num_games))
stan_data_ave$colour = c(stan_data_ave$colour, sim_colour)
stan_data_ave$elo = c(stan_data_ave$elo, c(sim_rating - sim_opp_rating))
stan_data_ave$win_prop = c(stan_data_ave$win_prop, sim_hist)




#now fit model on this player


#stan_file <- here("owen", "cluster_scripts", "final_model_scale_priors.stan")
stan_file <- here("code", "stan", "final_model_scale_priors_NEW.stan")

mod <- cmdstan_model(stan_file)

perm_fit <- mod$sample(data = stan_data_ave,
                      seed = 123,
                      chains = 4,
                      parallel_chains = 4,
                      refresh = 100)

perm_fit$save_object(file = here("results", "model fits", "sim testing", "23_25_fit_1_sim_player.RDS"))

#if the sim players winner effect isn't dead on zero then that would explain the bots having winner effects
  #I guess even randomly generated data sets that are truly independent will still have winner effects for some players...



