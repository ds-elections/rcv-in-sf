UsefulImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct)

for (i in c(1, 3, 5, 7, 9, 11)) {
  assign(paste0("District", i),
         UsefulImage %>%
           filter(contest == paste("Board of Supervisors, District", i, sep = " ")))
}

round0 <- data.frame(unique(District1$candidate))
colnames(round0) <- c("candidate")
loser0 <- data.frame(candidate = character())

a <- (nrow(round0) - 3)
for (i in 0:a) {
  assign(
    paste0("round", i+1),
    District1 %>%
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
    right_join(get(paste0("round", i+1)),
               get(paste0("round", i)),
               by = "candidate"))
}
get(paste0("round", a + 1))