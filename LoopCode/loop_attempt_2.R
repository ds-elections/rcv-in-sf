UsefulImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct)

for (j in c(1, 3, 5, 7, 9, 11)) {
  assign(paste0("District", j),
         UsefulImage %>%
           filter(contest == paste("Board of Supervisors, District", j, sep = " ")))
  
  
  # Could do another loop here to get data on every district, w/ something like:
  # paste("round", i, j, sep = ".") and above for loop
  
  assign(paste("round", "0", j, sep = "."), data.frame(unique(get(paste0("District", j))[ ,4])))
  assign(colnames(get(paste("round", "0", j, sep = "."))), c("candidate"))
  assign(paste("loser", "0", j, sep = "."), data.frame(candidate = character()))

a <- (nrow(get(paste("round", "0", j, sep = "."))) - 3)
for (i in 0:a) {
  assign(
    paste("round", i+1, j, sep = "."),
    get(paste0("District", j)) %>%
      filter(!(candidate %in% get(paste("loser", i, j, sep = "."))[['candidate']])) %>%
      group_by(pref_voter_id) %>%
      filter(vote_rank == min(vote_rank)) %>%
      ungroup() %>%
      group_by(candidate) %>%
      summarise(total = n()) %>%
      arrange(desc(total)))
  b <- get(paste("round", i+1, j, sep = ".")) %>% filter(!is.na(candidate))
  assign(paste("round", i+1, j, sep = "."),
         mutate(
           get(paste("round", i+1, j, sep = ".")),
           prop = total/sum(b$total)))
  assign(
    paste("loser", i+1, j, sep = "."),
    get(paste("round", i+1, j, sep = ".")) %>%
      arrange(total) %>%
      filter(!is.na(candidate)) %>%
      head(n = 1) %>%
      select(candidate) %>%
      rbind(get(paste("loser", i, j, sep = "."))))
  assign(
    paste("round", i+1, j, sep = "."),
    left_join(get(paste("round", i, j, sep = ".")),
              get(paste("round", i+1, j, sep = ".")),
              by = "candidate"))
}
assign(paste0("District", j, "Results"), get(paste("round", a+1, j, sep = ".")))
assign(paste0("District", j, "Results"), get(paste0("District", j, "Results"))[order(rowSums(is.na(get(paste0("District", j, "Results"))))), ])
assign(paste0("District", j, "Results"), get(paste0("District", j, "Results")) %>%
  arrange(is.na(candidate)))
}

rm(list = ls(pattern = "^loser"))
rm(list = ls(pattern = "^round"))

# Results are saved, and able to view
# need to fix colnames and remove NA proportions, as well as mutate to get transfer numbers