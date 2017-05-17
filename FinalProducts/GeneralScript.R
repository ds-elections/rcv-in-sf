library(tidyverse)
library(forcats)
BallotImage <- read_tsv("http://www.acgov.org/rov/rcv/results/230/BerkeleyMayor/ballot_image.txt",
                        # Insert link/file to ballot image above ^^
                        col_names = F) %>%
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
# Break into distinct columns
  
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

MasterLookup <- read_tsv("http://www.acgov.org/rov/rcv/results/230/BerkeleyMayor/master_lookup.txt",
                         # Insert link/file to master lookup file above ^^
                         col_names = F) %>%
  separate(X1, c("record_type",
                 "id",
                 "description",
                 "list_order",
                 "candidates_contest_id",
                 "is_writein",
                 "is_provisional"),
           sep = c(10,17,67,74,81,82)) %>%
# Break into distinct columns
  
  mutate(record_type = trimws(record_type),
         description = trimws(description),
         is_writein = as.integer(is_writein),
         is_provisional = as.integer(is_provisional))

# Prepare "key" for joining with ballot image
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

# Join key to ballot image for each voter
BallotImage <- left_join(BallotImage, Candidates, by = c("candidate_id" = "id"))
BallotImage <- left_join(BallotImage, Contests, by = c("contest_id" = "id"))
BallotImage <- left_join(BallotImage, Precincts, by = c("precinct_id" = "id"))
BallotImage <- left_join(BallotImage, Tallies, by = c("tally_type_id" = "id"))

FinalImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct) %>%
  spread(key = vote_rank, value = candidate)

# FinalImage is the nice table to look at

UsefulImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct)

for (j in unique(BallotImage$contest)) {
  
# Filter for only the current election
  assign(paste0("Election: ", j),
         UsefulImage %>%
           filter(contest == j))

# Create the base dataframes to attach results to round by round   
  assign("round0", data.frame(unique(get(paste0("Election: ", j))[ ,4])))
  assign(colnames(round0), c("candidate"))
  assign("loser0", data.frame(candidate = character()))
  
  a <- (nrow(round0) - 3)
  for (i in 0:a) {
    assign(
      paste0("round", i+1),
      get(paste0("Election: ", j)) %>%
        
# Remove all previous losers
        filter(!(candidate %in% get(paste0("loser", i))[['candidate']])) %>%
        group_by(pref_voter_id) %>%
  
# Pull the lowest vote_rank for each voter, their highest remaining choice
        filter(vote_rank == min(vote_rank)) %>%
        ungroup() %>%
        group_by(candidate) %>%
  
# Count vote totals
        summarise(total = n()) %>%
        arrange(desc(total)))
    
    b <- get(paste0("round", i+1)) %>% filter(!is.na(candidate))
    assign(paste0("round", i+1),
           mutate(
             get(paste0("round", i+1)),
             prop = total/sum(b$total)))
# Attach vote proportion in addition to total
    
    assign(
      paste0("loser", i+1),
      get(paste0("round", i+1)) %>%
        arrange(total) %>%
        filter(!is.na(candidate)) %>%
        head(n = 1) %>%
        select(candidate) %>%
        rbind(get(paste0("loser", i))))
# Determine the loser of that round
    
    assign(
      paste0("round", i+1),
      left_join(get(paste0("round", i)),
                get(paste0("round", i+1)),
                by = "candidate"))
# Attach the current round to the table of previous rounds
    
  }
  assign(paste0("Election: ", j, " (Results)"), get(paste0("round", a+1)))
  assign(paste0("Election: ", j, " (Results)"), get(paste0("Election: ", j, " (Results)"))[order(rowSums(is.na(get(paste0("Election: ", j, " (Results)"))))), ])
  assign(paste0("Election: ", j, " (Results)"), get(paste0("Election: ", j, " (Results)")) %>%
           arrange(is.na(candidate)))
# Arrange the table in order of elimination, with exhausted/invalid votes (NA) at the bottom
  
}

# Clear all the extra items out of the environment
rm(list = ls(pattern = "^loser"))
rm(list = ls(pattern = "^round"))

# Results are saved, and able to view