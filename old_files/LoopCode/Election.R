District1 <- FinalImage %>%
  filter(contest == "Board of Supervisors, District 1") %>%
  mutate(vote = `1`,
         `3` = ifelse(`3` == `1` |
                        `3` == `2`,
                      NA, `3`),
         `2` = ifelse(`2` == `1`, # Remove overvotes
                      NA, `2`))

Round1 <- District1 %>%
  group_by(vote) %>%
  summarise(Round1Total = n(),
            Round1Prop = Round1Total/nrow(District1)) %>%
  arrange(desc(Round1Total))
# Using the Certification Letter to verify my vote totals
# Round 1 works

Round2 <- District1 %>%
  mutate(vote = ifelse(is.na(vote),`2`,vote),
         vote = ifelse(is.na(vote),`3`,vote)) %>%
  group_by(vote) %>%
  summarise(Round2Total = n(),
            Round2Prop = Round2Total/nrow(District1)) %>%
  arrange(desc(Round2Total)) %>%
  right_join(Round1, by = "vote")

Round2 <- District1 %>%
  mutate(vote = ifelse(vote == as.character(Round1[nrow(Round1),1]),`2`,vote)) %>%
  group_by(vote) %>%
  summarise(Round2Total = n(),
            Round2Prop = Round2Total/nrow(District1)) %>%
  arrange(desc(Round2Total)) %>%
  right_join(Round1, by = "vote")
# Round 2 has more votes than expected for most of the candidates. What's up?

