data <- BallotImage %>%
  select(pref_voter_id, precinct, contest, over_vote, under_vote, serial_number) %>%
  group_by(pref_voter_id) %>%
  summarise(over = (sum(over_vote) != 0),
            under = (sum(under_vote) != 0),
            precinct = unique(precinct),
            contest = unique(contest),
            serial_number = unique(serial_number))

PrecinctData <- data %>%
  group_by(precinct) %>%
  summarise(p_over = mean(over),
            p_under = mean(under),
            contest = unique(contest))