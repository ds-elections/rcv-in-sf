library(tidyverse)

UsefulImage <- BallotImage %>%
  select(pref_voter_id, contest, vote_rank, candidate, precinct)

for (j in c(1, 3, 5, 7, 9, 11)) {
  assign(paste0("District", j),
         UsefulImage %>%
           filter(contest == paste("Board of Supervisors, District", j, sep = " ")))
}

# Could do another loop here to get data on every district, w/ something like:
# paste("round", j, i, sep = ".") and above for loop

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
    left_join(get(paste0("round", i)),
               get(paste0("round", i+1)),
               by = "candidate"))
}
results <- get(paste0("round", a + 1))
results <- results[order(rowSums(is.na(results))), ]
results <- results %>%
  arrange(is.na(candidate))

View(results)
  
# Results are saved, and able to view
# need to fix colnames and remove NA proportions, as well as mutate to get transfer numbers