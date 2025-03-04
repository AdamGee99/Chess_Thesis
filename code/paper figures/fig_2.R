library(tidyverse)
library(here)
library(MASS)
library(scales)


### FUNCTIONS ###

#function to read in separate files and merge
read_player <- function(path, file){
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
theme_2col <- function(){ 
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.spacing.x = unit(1, "lines"),
      
      #text elements
      plot.title = element_text(size = title_size),
      axis.text = element_text(size = axis_text_size - 1.25),
      axis.title = element_text(size = axis_title),
      legend.text = element_text(size = legend_text),
      strip.text = element_text(size = axis_title - 3.5, 
                                margin = margin(1.5, 1.5, 2, 1.5)),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
}


### IMPORT DATA ###

# 1700-1900 #
data_path = here("code for paper figures", "lichess1700-1900")

files <- list.files(data_path)

lichess_data_amateur <- files %>%
  map_dfr(~read_player(data_path, .x))

#selecting bullet 60+0 only
bullet_60_amateur <- lichess_data_amateur %>% 
  filter(Event == "Rated Bullet game") %>% 
  filter(TimeControl == "60+0") %>%
  dplyr::select(c("White", "Black", "Result", "UTCDate", "UTCTime", "Username", "WhiteElo", "BlackElo"))

#players with over 100 games played
amateur_players = bullet_60_amateur %>% group_by(Username) %>% 
  summarise(n = n()) %>% filter(n > 100) %>% dplyr::select(Username) %>% unlist() %>% unname()


# GMs #
data_path = here("code for paper figures", "lichessGrandmasters")
files <- list.files(data_path)
lichess_data_gms <- files %>%
  map_dfr(~read_player(data_path, .x))

#selecting bullet 60+0 only
bullet_60_gms <- lichess_data_gms %>% 
  filter(Event == "Rated Bullet game") %>% 
  filter(TimeControl == "60+0") %>%
  dplyr::select(c("White", "Black", "Result", "UTCDate", "UTCTime", "Username", "WhiteElo", "BlackElo"))

#players with over 100 games played
gm_players = bullet_60_gms %>% group_by(Username) %>% 
  summarise(n = n()) %>% filter(n > 100) %>% dplyr::select(Username) %>% pull()





### TIDYING ###


# 1700-1900 #

#storage
amateur_players_elo = list()
amateur_session_lengths = list()
amateur_time_diffs = list()
amateur_time_diffs_10 = list()

for (i in 1:length(amateur_players)) {
  
  tidy_games_amateur = bullet_60_amateur %>% 
    filter(Username == amateur_players[i]) %>% #select only one player
    mutate(UTCDateTime = ymd_hms(paste(UTCDate, UTCTime))) %>% #convert to lubridate type
    arrange(UTCDateTime) %>% #order date-time
    mutate(time_diff = UTCDateTime - lag(UTCDateTime)) #time difference between games
  
  #some players have all their games double recorded for some reason
  tidy_games_amateur = tidy_games_amateur[!duplicated(tidy_games_amateur[1:9]),] #remove duplicate rows
  
  #get players average elo across all games
  amateur_players_elo[[amateur_players[i]]] = mean(c(tidy_games_amateur$WhiteElo[amateur_players[i] == tidy_games_amateur$White], 
                                                     tidy_games_amateur$BlackElo[amateur_players[i] == tidy_games_amateur$Black]))
  
  # #histogram of all times between games
  # hist(as.numeric(tidy_games_amateur$time_diff[tidy_games_amateur$time_diff < 500]),
  #      breaks = 50, xlab = "Time Between Games (Sec)",
  #      main = paste0("Time Difference Between Games for ", amateur_players[i]))
  
  #most back to back games seem to be played under 300 sec apart
  
  #store time diffs between games
  amateur_time_diffs[[amateur_players[i]]] = difftime(tidy_games_amateur$UTCDateTime, 
                                                      lag(tidy_games_amateur$UTCDateTime), units = "secs") #time between current game and previous
  amateur_time_diffs_10[[amateur_players[i]]] = difftime(tidy_games_amateur$UTCDateTime, 
                                                         lag(tidy_games_amateur$UTCDateTime, 10), units = "secs") #time between current game and 10th game ago
}
#store in df for plotting
amateur_time_diff_df = data.frame("time_diff" = unlist(amateur_time_diffs))
amateur_time_diff_df_10 = data.frame("time_diff" = unlist(amateur_time_diffs_10))

### Proceed by assuming all games played within 300 seconds (5 min) are in the same session
#getting session lengths for each player
for (i in 1:length(amateur_players)) {
  tidy_games_amateur = bullet_60_amateur %>% 
    filter(Username == amateur_players[i]) %>% #select only one player
    mutate(UTCDateTime = ymd_hms(paste(UTCDate, UTCTime))) %>% #convert date-time to lubridate type
    arrange(UTCDateTime) %>% #order date-time
    mutate(time_diff = UTCDateTime - lag(UTCDateTime)) #time difference between games
  
  length_of_sessions =  which(tidy_games_amateur$time_diff > 300) - lag(which(tidy_games_amateur$time_diff > 300), default = 0) 
  #game id of first game of session minus game id of first game of previous session
  
  amateur_session_lengths[[amateur_players[i]]] = length_of_sessions #store
}
amateur_session_lengths_df = data.frame(session_length = unlist(amateur_session_lengths))


# GMs #

#storage
gm_players_elo = list()
gm_session_lengths = list()
gm_time_diffs = list()
gm_time_diffs_10 = list()

for (i in 1:length(gm_players)) {
  
  tidy_games_gms = bullet_60_gms %>% 
    filter(Username == gm_players[i]) %>% #select only one player
    mutate(UTCDateTime = ymd_hms(paste(UTCDate, UTCTime))) %>% #convert date-time to lubridate type
    arrange(UTCDateTime) %>% #order date-time
    mutate(time_diff = UTCDateTime - lag(UTCDateTime)) #time difference between games
  
  
  #some players have all their games double recorded for some reason
  tidy_games_gms = tidy_games_gms[!duplicated(tidy_games_gms[1:9]),] #remove duplicate rows
  
  #get players average elo across all games
  gm_players_elo[[gm_players[i]]] = mean(c(tidy_games_gms$WhiteElo[gm_players[i] == tidy_games_gms$White], 
                                           tidy_games_gms$BlackElo[gm_players[i] == tidy_games_gms$Black]))
  
  # #histogram of all times between games
  # hist(as.numeric(tidy_games_gms$time_diff[tidy_games_gms$time_diff < 500]),
  #      breaks = 50, xlab = "Time Between Games (Sec)",
  #      main = paste0("Time Difference Between Games for ", gm_players[i]))
  
  #store time diffs between games
  gm_time_diffs[[gm_players[i]]] = difftime(tidy_games_gms$UTCDateTime, 
                                            lag(tidy_games_gms$UTCDateTime), units = "secs") #time between current game and previous
  gm_time_diffs_10[[gm_players[i]]] = difftime(tidy_games_gms$UTCDateTime, 
                                               lag(tidy_games_gms$UTCDateTime, 10), units = "secs") #time between current game and 10th game ago
}
#store in df for plotting
gm_time_diff_df = data.frame("time_diff" = unlist(gm_time_diffs))
gm_time_diff_df_10 = data.frame("time_diff" = unlist(gm_time_diffs_10))

### Same distributions as amateurs, so again proceed by assuming all games played within 300 seconds (5 min) are in the same session
for (i in 1:length(gm_players)) {
  tidy_games_gms = bullet_60_gms %>% 
    filter(Username == gm_players[i]) %>% #select only one player
    mutate(UTCDateTime = ymd_hms(paste(UTCDate, UTCTime))) %>% #convert date-time to lubridate type
    arrange(UTCDateTime) %>% #order date-time
    mutate(time_diff = UTCDateTime - lag(UTCDateTime)) #time difference between games
  
  length_of_sessions =  which(tidy_games_gms$time_diff > 300) - lag(which(tidy_games_gms$time_diff > 300), default = 0) 
  #game id of first game of session minus game id of first game of previous session
  
  gm_session_lengths[[gm_players[i]]] = length_of_sessions #store
}
#store in df for plotting
gm_session_lengths_df = data.frame(session_length = unlist(gm_session_lengths))




### PLOTTING ###

axis_title = 16
title_size = 18
axis_text_size = 14
legend_text = 14
line_size = 1
theme_set(theme_2col())

# 1700-1900 #

#log time between games
log_time_diff_plot_amateur = ggplot(amateur_time_diff_df, aes(x = as.numeric(time_diff))) +
  geom_histogram() +
  scale_x_continuous(trans = scales::log_trans(),
                     breaks = c(1, 60, 300, 3600, 86400, 31536000),
                     labels = c("1 sec", "1 min", "5 min", "1 hour", "1 day", "1 year")) +
  xlab("Time Between Games (Log Scale)") +
  geom_vline(xintercept = 300, colour = "red") +
  ylab("Frequency") #+ ggtitle("Time Difference Between Current Game and Previous Game for Amateurs")
log_time_diff_plot_amateur

#log time between curr game and 10th game ago
log_time_diff_plot_10_amateur = ggplot(amateur_time_diff_df_10, aes(x = as.numeric(time_diff))) +
  geom_histogram() +
  scale_x_continuous(trans = scales::log_trans(),
                     breaks = c(1, 60, 300, 3600, 86400, 31536000),
                     labels = c("1 sec", "1 min", "5 min", "1 hour", "1 day", "1 year")) +
  xlab("Time Between Current Game and 10th Game Ago (Log Scale)") +
  ylab("Frequency") #+ ggtitle("Time Difference Between Current Game and 10th Game Ago for Amateurs")
log_time_diff_plot_10_amateur

#log number of games per session
session_length_plot_amateur = ggplot(amateur_session_lengths_df, aes(x = session_length)) +
  geom_histogram() +
  scale_x_continuous(trans = pseudo_log_trans(sigma = 2.36),
                     breaks = c(1, 5, 10, 100),
                     labels = c("1 game", "5 games", "10 games", "100 games"),
                     limits = c(0, 389)) +
  xlab("Number of Games in a Session (Log Scale)") +
  ylab("Frequency") #+ ggtitle("Number of Games per Session for GMs")
session_length_plot_amateur


# GMs #

#log time between games
log_time_diff_plot_gm = ggplot(gm_time_diff_df, aes(x = as.numeric(time_diff))) +
  geom_histogram() +
  scale_x_continuous(trans = scales::log_trans(),
                     breaks = c(1, 60, 300, 3600, 86400, 31536000),
                     labels = c("1 sec", "1 min", "5 min", "1 hour", "1 day", "1 year")) +
  xlab("Time Between Games (Log Scale)") +
  geom_vline(xintercept = 300, colour = "red") +
  ylab("Frequency") #+ ggtitle("Time Difference Between Current Game and Previous Game for Amateurs")
log_time_diff_plot_gm #this is the one used in the paper

#log time between curr game and 10th game ago
log_time_diff_plot_10_gm = ggplot(gm_time_diff_df_10, aes(x = as.numeric(time_diff))) +
  geom_histogram() +
  scale_x_continuous(trans = scales::log_trans(),
                     breaks = c(1, 60, 300, 3600, 86400, 31536000),
                     labels = c("1 sec", "1 min", "5 min", "1 hour", "1 day", "1 year")) +
  xlab("Time Between Current Game and 10th Game Ago (Log Scale)") +
  ylab("Frequency") #+ ggtitle("Time Difference Between Current Game and 10th Game Ago for Amateurs")
log_time_diff_plot_10_gm

#log number of games per session
session_length_plot_gm = ggplot(gm_session_lengths_df, aes(x = session_length)) +
  geom_histogram() +
  scale_x_continuous(trans = pseudo_log_trans(sigma = 2.36),
                     breaks = c(1, 10, 100),
                     labels = c("1 game", "10 games", "100 games"),
                     limits = c(0, 389)) +
  xlab("Number of Games in a Session (Log Scale)") +
  ylab("Frequency") #+ ggtitle("Number of Games per Session for GMs")
session_length_plot_gm #this is the one in the paper



### SAVING FIGURES ###

save_path = here("code for paper figures")

ggsave(plot = log_time_diff_plot_gm,
       filename = paste0(save_path, "/time_diff_plot.png"),
       device = "png", dpi = 1000, width = 6, height = 4)
ggsave(plot = session_length_plot_gm,
       filename = paste0(save_path, "/session_length_plot.png"),
       device = "png", dpi = 1000, width = 6, height = 4)





