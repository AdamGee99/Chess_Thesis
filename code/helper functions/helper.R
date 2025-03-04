#### July 2024 
## Collection of helper functions for processing data
##


read_player <- function(path, file){
  dat <- read_csv(file = paste0(path, file),
                  col_types = cols(UTCDate = col_date("%Y.%m.%d"),
                                   WhiteTitle = col_character(),
                                   BlackTitle = col_character(),
                                   WhiteElo = col_character(),
                                   BlackElo = col_character(),
                                   FEN = col_character())) |> 
    dplyr::select(Username, Event, White, Black, Result, UTCDate, UTCTime, 
                  WhiteElo, BlackElo, Variant, TimeControl, Termination) |> 
    mutate(WhiteElo = parse_number(if_else(WhiteElo == "?", NA, WhiteElo)),
           BlackElo = parse_number(if_else(BlackElo == "?", NA, BlackElo)))
  dat
}

get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games |> 
      # filter(White == user | Black == user) |>
      filter(Username == user) |> 
      arrange(UTCDate, UTCTime) |> 
      mutate(focal_white = ifelse(Username == White, 1, 0)) |> 
      select(White:BlackElo, focal_white) |> 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) |> 
      mutate(focal_win_prop = focal_result)
    
  }
  else{
    hist_games <- games |> 
      # filter(White == user | Black == user) |> 
      filter(Username == user) |> 
      arrange(UTCDate, UTCTime) |> 
      mutate(focal_white = ifelse(Username == White, 1, 0)) |> 
      select(White:BlackElo, focal_white) |> 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) |> 
      mutate(focal_win_prop = c(cumsum(focal_result[1:(prev_n - 1)])/(1:(prev_n -1)), 
                                roll_mean(focal_result, n = prev_n)))
  }
  
  hist_games
}

## extract the sessions containing more than 1 game
get_true_sequences <- function(x) {
  rle_x <- rle(x)
  true_lengths <- rle_x$lengths[rle_x$values == TRUE]
  return(true_lengths)
}



## plotting defaults
## setup for the plots
theme_set(theme_bw())
axis_title <- 16
title_size <- 18
axis_text_size <- 14
legend_text <- 14
line_size <- 1



#for reading in PGNs
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