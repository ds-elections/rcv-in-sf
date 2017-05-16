# Works in the case of "California style" ballot images
library(tidyverse)
library(forcats)

UsefulImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct)

## loop that generates results for all contests in a properly formatted ballot image

for (j in unique(UsefulImage$contest)) {
  assign(paste0("Election: ", j),
         UsefulImage %>%
           filter(contest == j))
  
  assign("round0", data.frame(unique(get(paste0("Election: ", j))['candidate'])))
  colnames(round0) <- c("candidate")
  assign("loser0", data.frame(candidate = character()))
  
  a <- (nrow(round0) - 3)
  for (i in 0:a) {
    assign(
      paste0("round", i+1),
      get(paste0("Election: ", j)) %>%
        filter(!(candidate %in% get(paste0("loser", i))[['candidate']])) %>%
        group_by(pref_voter_id) %>%
        filter(vote_rank == min(vote_rank)) %>%
        ungroup() %>%
        group_by(candidate) %>%
        summarise(total = n()) %>%
        arrange(desc(total)))
    b <- get(paste0("round", i+1)) %>% filter(!is.na(candidate))
    assign(paste0("round", i+1),
           mutate(
             get(paste0("round", i+1)),
             prop = total/sum(b$total)))
    assign(
      paste0("loser", i+1),
      get(paste0("round", i+1)) %>%
        arrange(total) %>%
        filter(!is.na(candidate)) %>%
        head(n = 1) %>%
        select(candidate) %>%
        rbind(get(paste0("loser", i))))
    temprcvround <- get(paste0("round", i+1))
    rcvcolnames <- c(paste0("candidate"), paste0("round", i+1, "total"), paste0("round", i+1, "prop"))
    colnames(temprcvround) <- rcvcolnames
    assign(paste0("round", i+1), temprcvround)
    assign(
      paste0("round", i+1),
      left_join(get(paste0("round", i)),
                get(paste0("round", i+1)),
                by = "candidate"))
  }
  assign(paste0("Election: ", j, " (Results)"), get(paste0("round", a+1)))
  assign(paste0("Election: ", j, " (Results)"), get(paste0("Election: ", j, " (Results)"))[order(rowSums(is.na(get(paste0("Election: ", j, " (Results)"))))), ])
  assign(paste0("Election: ", j, " (Results)"), get(paste0("Election: ", j, " (Results)")) %>%
           arrange(is.na(candidate)))
}

rm(list = ls(pattern = "^loser"))
rm(list = ls(pattern = "^round"))
rm(rcvcolnames)
rm(temprcvround)

## Function that returns a dataframe given a properly formatted ballot image and a selected contest
## example: mayor.election <- rcv_results(rcvimage = FinalImage, rcvcontest = "Mayor - Berkeley (RCV)")

rcv_results <- function(rcvimage, rcvcontest) {
  
  UsefulImage <- rcvimage %>%
    select(pref_voter_id, contest, vote_rank, candidate, precinct)

  assign(paste0("Election: ", rcvcontest),
         UsefulImage %>%
           filter(contest == rcvcontest))
  
  assign("round0", data.frame(unique(get(paste0("Election: ", rcvcontest))['candidate'])))
  colnames(round0) <- c("candidate")
  assign("loser0", data.frame(candidate = character()))
  
  a <- (nrow(round0) - 3)
  for (i in 0:a) {
    assign(
      paste0("round", i+1),
      get(paste0("Election: ", rcvcontest)) %>%
        filter(!(candidate %in% get(paste0("loser", i))[['candidate']])) %>%
        group_by(pref_voter_id) %>%
        filter(vote_rank == min(vote_rank)) %>%
        ungroup() %>%
        group_by(candidate) %>%
        summarise(total = n()) %>%
        arrange(desc(total)))
    b <- get(paste0("round", i+1)) %>% filter(!is.na(candidate))
    assign(paste0("round", i+1),
           mutate(
             get(paste0("round", i+1)),
             prop = total/sum(b$total)))
    assign(
      paste0("loser", i+1),
      get(paste0("round", i+1)) %>%
        arrange(total) %>%
        filter(!is.na(candidate)) %>%
        head(n = 1) %>%
        select(candidate) %>%
        rbind(get(paste0("loser", i))))
    temprcvround <- get(paste0("round", i+1))
    rcvcolnames <- c(paste0("candidate"), paste0("round", i+1, "total"), paste0("round", i+1, "prop"))
    colnames(temprcvround) <- rcvcolnames
    assign(paste0("round", i+1), temprcvround)
    assign(
      paste0("round", i+1),
      left_join(get(paste0("round", i)),
                get(paste0("round", i+1)),
                by = "candidate"))
  }
  assign(paste0("Election: ", rcvcontest, " (Results)"), get(paste0("round", a+1)))
  assign(paste0("Election: ", rcvcontest, " (Results)"), get(paste0("Election: ", rcvcontest, " (Results)"))[order(rowSums(is.na(get(paste0("Election: ", rcvcontest, " (Results)"))))), ])
  assign(paste0("Election: ", rcvcontest, " (Results)"), get(paste0("Election: ", rcvcontest, " (Results)")) %>%
           arrange(is.na(candidate)))

  return(get(paste0("Election: ", rcvcontest, " (Results)")))

}

## Start of a function that will generate a sankey plot given a single properly formatted results dataframe

rcv_sankey_plot <- function(rcvresults) {

  d <- (ncol(rcvresults)-1)/2
  assign("transfer1", data.frame(rcvresults$candidate, rcvresults$round1total))
  colnames(transfer1) <- c("candidate", "transfer")
  for(i in 1:d) {
    assign(paste0("transfer", i+1), 
           data.frame(rcvresults$candidate, rcvresults[paste0("round", i+1, "total")] - rcvresults[paste0("round", i+1, "total")]))
    temptransfer <- get(paste0("transfer", i+1))
    colnames(temptransfer) <- c("candidate", "transfer")
    assign(paste0("transfer", i+1), temptransfer)
    assign(paste0("transfer", i+1),
           left_join(get(paste0("transfer", i)), get(paste0("transfer", i+1)), by = "candidate"))
  }
  assign(sankeytransfer, get("transfer", d+1))
  return(sankeytransfer)
}
  
  
  
