# Works in the case of "California style" ballot images
library(tidyverse)
library(forcats)
BallotImage <- read_tsv("http://www.acgov.org/rov/rcv/results/230/BerkeleyMayor/ballot_image.txt", col_names = F) %>%
  separate(X1, c("contest_id",
                 "pref_voter_id",
                 "serial_number",
                 "tally_type_id",
                 "precinct_id",
                 "vote_rank",
                 "candidate_id",
                 "over_vote",
                 "under_vote"),
           sep = c(7,16,23,26,33,36,43,44)) %>%
  mutate(tally_type_id = as.integer(tally_type_id),
         vote_rank = factor(vote_rank,
                            ordered = T,
                            levels = c("001","002","003")),
         vote_rank = fct_recode(vote_rank,
                                "1" = "001",
                                "2" = "002",
                                "3" = "003"),
         over_vote = as.integer(over_vote),
         under_vote = as.integer(under_vote))

MasterLookup <- read_tsv("http://www.acgov.org/rov/rcv/results/230/BerkeleyMayor/master_lookup.txt", col_names = F) %>%
  separate(X1, c("record_type",
                 "id",
                 "description",
                 "list_order",
                 "candidates_contest_id",
                 "is_writein",
                 "is_provisional"),
           sep = c(10,17,67,74,81,82)) %>%
  mutate(record_type = trimws(record_type),
         description = trimws(description),
         is_writein = as.integer(is_writein),
         is_provisional = as.integer(is_provisional))

Candidates <- MasterLookup %>%
  filter(record_type == "Candidate") %>%
  select(id, description) %>%
  rename(candidate = description)
Contests <- MasterLookup %>%
  filter(record_type == "Contest") %>%
  select(id, description) %>%
  rename(contest = description)
Precincts <- MasterLookup %>%
  filter(record_type == "Precinct") %>%
  select(id, description) %>%
  rename(precinct = description)
Tallies <- MasterLookup %>%
  filter(record_type == "Tally Type") %>%
  select(id, description) %>%
  mutate(id = as.integer(id)) %>%
  rename(tally = description)

BallotImage <- left_join(BallotImage, Candidates, by = c("candidate_id" = "id"))
BallotImage <- left_join(BallotImage, Contests, by = c("contest_id" = "id"))
BallotImage <- left_join(BallotImage, Precincts, by = c("precinct_id" = "id"))
BallotImage <- left_join(BallotImage, Tallies, by = c("tally_type_id" = "id"))

FinalImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct) %>%
  spread(key = vote_rank, value = candidate)

# FinalImage is the nice file. We can just run means on it to calculate results.

UsefulImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct)

for (j in unique(BallotImage$contest)) {
  assign(paste0("Election: ", j),
         UsefulImage %>%
           filter(contest == j))
  
  assign("round0", data.frame(unique(get(paste0("Election: ", j))[ ,4])))
  assign(colnames(round0), c("candidate"))
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

# Results are saved, and able to view
# need to fix colnames and remove NA proportions, as well as mutate to get transfer numbers