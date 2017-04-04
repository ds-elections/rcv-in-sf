library(tidyverse)
BallotImage <- read_tsv("data/20161206_ballotimage.txt", col_names = F) %>%
  separate(X1, c("contest_id",
                 "pref_voter_id",
                 "serial_number",
                 "tally_type_id",
                 "precinct_id",
                 "vote_rank",
                 "candidate_id",
                 "over_vote",
                 "under_vote"),
           sep = c(7,16,23,26,33,36,43,44))

MasterLookup <- read_tsv("data/20161206_masterlookup.txt", col_names = F) %>%
  separate(X1, c("record_type",
                 "id",
                 "description",
                 "list_order",
                 "candidates_contest_id",
                 "is_writein",
                 "is_provisional"),
           sep = c(10,17,67,74,81,82))

