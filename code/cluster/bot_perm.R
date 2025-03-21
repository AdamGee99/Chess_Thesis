#### Feb 13, 2025

#cluster script
#creating permutation distributions for the bots



library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)
library(bigchess) #package for reading in PGNs

options(mc.cores = parallel::detectCores())

#function to read in separate files and merge
read.pgn = function (con, add.tags = NULL, n.moves = T, extract.moves = 10, 
                     last.move = T, stat.moves = T, big.mode = F, quiet = F, ignore.other.games = F, 
                     source.movetext = F) 
{
  st <- Sys.time()
  tags <- c(c("Event", "Site", "Date", "Round", "White", "Black", 
              "Result", "WhiteElo", "BlackElo", "ECO", "Opening", "Variation"), add.tags)
  if (big.mode) 
    al = con
  else {
    al = readLines(con)
    if ("connection" %in% class(con)) 
      close(con)
  }
  s <- "^\\[([\\S]+)\\s\"([\\S\\s]+|\\B)\"\\]$"
  tmp1 <- gsub(s, "\\1", al, perl = T)
  tmp2 <- gsub(s, "\\2", al, perl = T)
  tmp3 <- grepl("^\\[[^%]+\\]$", al, perl = T)
  tmp4 <- cumsum(grepl("\\[Event ", al))
  tmp1[!tmp3] <- "Movetext"
  r2 <- data.frame(tmp1, tmp2, tmp3, tmp4, stringsAsFactors = F)
  gt <- paste(subset(r2, tmp1 == "Movetext", select = c(tmp2))[, 
                                                               1], collapse = " ")
  if (source.movetext) 
    gt2 <- gt
  gt <- gsub("{[^}]+}", "", gt, perl = T)
  gt <- gsub("\\((?>[^()]|(?R))*\\)", "", gt, perl = T)
  gt <- gsub("[\\?\\!]", "", gt, perl = T)
  gt <- gsub("[0-9]+\\.\\.\\.", "", gt, perl = T)
  gt <- gsub("\\$[0-9]+", "", gt, perl = T)
  for (i in c("1-0", "1\\/2-1\\/2", "0-1", "\\*")) gt <- unlist(strsplit(gt, 
                                                                         split = i))
  if (source.movetext) 
    for (i in c("1-0", "1\\/2-1\\/2", "0-1", "\\*")) gt2 <- unlist(strsplit(gt2, 
                                                                            split = i))
  r <- subset(r2, tmp1 == "Event", select = c(tmp4, tmp2))
  colnames(r) <- c("GID", "Event")
  for (i in c(setdiff(tags, "Event"))) {
    tmp <- subset(r2, tmp1 == i, select = c(tmp4, tmp2))
    colnames(tmp) <- c("GID", i)
    r <- merge(r, tmp, all.x = T)
  }
  r$Movetext <- trimws(gsub("[[:space:]]+", " ", head(gt, nrow(r))))
  if (source.movetext) 
    r$SourceMovetext <- head(gt2, nrow(r))
  tal <- tail(al, 1)
  if (big.mode) 
    if (!grepl("\\[", tal) & !(tal == "")) {
      r[nrow(r), "Movetext"] <- ""
    }
  if (big.mode) 
    if (r[nrow(r), "Movetext"] == "") 
      r <- r[-nrow(r), ]
  if (!quiet) 
    message(paste0(Sys.time(), ", successfully imported ", 
                   nrow(r), " games"))
  if (n.moves || extract.moves) 
    r$NMoves <- n_moves(r$Movetext)
  if (!quiet) 
    message(paste0(Sys.time(), ", N moves computed"))
  if (extract.moves) {
    if (extract.moves == -1) {
      N <- max(r$NMoves)
    }
    else {
      N <- extract.moves
    }
    r <- cbind(r, extract_moves(r$Movetext, N, last.move = last.move))
    if (!quiet) 
      message(paste0(Sys.time(), ", extract moves done"))
  }
  if (stat.moves) {
    r <- cbind(r, stat_moves(r$Movetext))
    if (!quiet) 
      message(paste0(Sys.time(), ", stat moves computed"))
  }
  if (ignore.other.games) {
    nr <- nrow(r)
    r <- subset(r, Result != "*")
    r$Result <- factor(r$Result, levels = c("1-0", "1/2-1/2", 
                                            "0-1"), labels = c("1-0", "1/2-1/2", "0-1"), ordered = T)
    if (!quiet) 
      message(paste0(Sys.time(), ", subset done (", nr - 
                       nrow(r), " games with Result '*' removed) "))
  }
  else {
    r$Result <- factor(r$Result, levels = c("1-0", "1/2-1/2", 
                                            "0-1", "*"), labels = c("1-0", "1/2-1/2", "0-1", 
                                                                    "*"), ordered = T)
  }
  r <- droplevels(r)
  for (i in intersect(colnames(r), c("WhiteElo", "BlackElo", 
                                     "SetUp"))) r[, i] <- as.integer(r[, i])
  
  #add focal enging column - find the engine that plays in all white or black games, thats the focal engine
  r$Username =  names(which((table(r$White) + table(r$Black)) == nrow(r)))
  
  r = r %>% dplyr::select(Username, Event, Round, White, Black, Result, WhiteElo, BlackElo, ECO, Opening, Variation)
  
  return(r)
}
get_hist = function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games %>% 
      filter(Username == user) %>%
      #arrange(Round) %>% 
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
      #arrange(UTCDate, UTCTime) %>% 
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


set_cmdstan_path("/home/adamgee/R/x86_64-pc-linux-gnu-library/4.4/cmdstan")


#for running array jobs 
jobid = Sys.getenv("SLURM_ARRAY_TASK_ID") %>% as.numeric()


data_path = "/home/adamgee/scratch/data/CCRL_bots/"
save_path = "/home/adamgee/scratch/results/bot_perm/"

#dir.create(save_path, showWarnings = FALSE)

files = list.files(data_path)

#raw pgn data
engine_blitz_raw = files %>% map_dfr(~read.pgn(paste0(data_path, "/", .x)))

#cleaning and arranging by round
engine_blitz = engine_blitz_raw %>% 
  arrange(Username, Round, .by_group = TRUE)

#users means engine names
users = engine_blitz$Username %>% unique()


### PERMUTE ###
engine_blitz = engine_blitz[sample(nrow(engine_blitz), size = nrow(engine_blitz), replace = FALSE),] %>% 
  arrange(Username)



tidy_games = map_dfr(users, get_hist, engine_blitz, prev_n = 1) %>% 
  as_tibble()


init_data = tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  mutate(rating_diff = ifelse(focal_white == 1,
                              WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  dplyr::select(focal_user, Round, focal_id, focal_white,
                focal_win_prop, rating_diff, focal_result, White, Black, WhiteElo, BlackElo) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  filter(focal_result != 0.5)


stan_data_ave = list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$rating_diff,
                      win_prop = init_data$ave_prop)


stan_file = "/home/adamgee/scratch/code/final_model_scale_priors_NEW.stan"

mod = cmdstan_model(stan_file)

bot_fit = mod$sample(data = stan_data_ave,
                      seed = 123,
                      chains = 4,
                      parallel_chains = 4,
                      refresh = 100)


#find and save the parameters of interest
par_ests = bot_fit$summary(c("beta", "mu_beta", "gamma1", "gamma2")) %>% 
  mutate(jobid = jobid)


saveRDS(par_ests, file = paste0(save_path, "perm_", jobid, ".RDS"))





