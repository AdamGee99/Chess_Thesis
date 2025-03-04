library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(scales)
library(ggridges)
library(here)
library(ggdist)
library(viridis)
library(PlayerRatings)
library(ggrepel)
library(ggpubr)


source(here("code", "helper functions", "helper.R"))
source(here("code", "helper functions", "plot_templates.R"))


#getting true values for winner effect
bot_fit = readRDS(here("results", "model fits", "bot fits", "fit1.RDS"))
users = readRDS(here("results", "model fits", "users", "bots_CCRL.RDS"))

true_winner = bot_fit$summary() %>% 
  dplyr::select(variable, mean) %>%
  filter(startsWith(variable, "beta[2,")) %>% 
  mutate(user_id = str_extract(variable, pattern = "(?<=,)[0-9]+")) %>%
  mutate(user = users[as.numeric(user_id)]) %>%
  dplyr::select(-c(user_id))

#read in perm results 
perm_data_path = here("results", "model fits", "bot perm fits")
perm_files = list.files(perm_data_path)

perm_bots_raw = perm_files %>%
  map_dfr(~read_rds(here(perm_data_path, .x)))

#select only variables, mean, and the sim, filter for winner effect
perm_bots = perm_bots_raw %>%
  dplyr::select(variable, mean, jobid) %>%
  rename(sim = jobid) %>% 
  filter(startsWith(variable, "beta[2")) %>%
  mutate(user_id = str_extract(variable, pattern = "(?<=,)[0-9]+")) %>%
  mutate(user = users[as.numeric(user_id)]) %>%
  dplyr::select(-c(user_id))
  
#for ordering factor variable in plot by mean
levels = true_winner %>%
  arrange(mean) %>%
  pull(user)

true_winner = true_winner %>%
  mutate(user = factor(user, levels = levels)) %>%
  arrange(user) %>%
  mutate(y_fake = row_number() - 0.25,
         yend = row_number() + 0.25)

#plotting
perm_bot_winner_plot = perm_bots %>%
  mutate(user = factor(user, levels = levels)) %>%
  ggplot(aes(x = mean, y = user)) +
  stat_histinterval(slab_fill = NA, .width = c(0.5, 0.95)) +
  geom_segment(data = true_winner,
               aes(x = mean, y = y_fake, yend = yend), col = "red") +
  theme_single_y() +
  labs(x = "Estimated Experiential Effect", y = "")
perm_bot_winner_plot

# ggsave(plot = perm_bot_winner_plot,
#        filename = here("results", "bot figures", "fit1", "500_perm_winner.png"), dpi = 1000,
#        height = 5, width = 7)








  

