#' March 11, 2025
#' Extracting bot data from some of the strongest and most popular bots used on Lichess
#' These bots have accounts on lichess, which is what allows us to extract with chessR package
#' They can play anyone (human or bot)
#' The important thing is they play from a random pool of players
#' This is ideal cause it matches our player data and eliminates the issue of having a fixed rotating schedule of opponents that you see with bot tournament data


library(chessR)
library(tidyverse)


#' unfortunately the lichess api is capped at 15 games per second
#' therefore will extract 900 games per minute, 54,000 per hour
#' most bots have over 10,000 games, so it will take a while...

#found bots from https://github.com/TheYoBots/libot-lb/blob/master/bot_leaderboard/bullet.md
  #list of the strongest bots in each time control and variant
  #randomly selected 10 of the strongest ones that had a lot of games
  #some have too many games though...

bot_usernames = c("IA_Horus", "LeelaMultiPoss", "TheMatrix2029", "que-encrypt", "caissa-x", 
                  "PatriciaBot", "Mr-Chess-Berserk", "ElPeonElectrico", "KunkkaBot", "duchessAI")

#extract games
bot_games = chessR::get_raw_lichess(bot_usernames)

#remove moves column, we don't need it and it takes up so much space
bot_games = bot_games %>% select(-Moves)

#save it
write.csv(bot_games, file = "/home/adamgee/scratch/data/lichessBots/lichess_bot_games.csv", row.names = FALSE)





