District1 <- FinalImage %>%
  filter(contest == "Board of Supervisors, District 1",
         !is.na(`1`))
Results <- District1 %>%
  group_by(`1`) %>%
  summarise(Round1Total = n(),
            Round1Prop = Round1Total/nrow(District1)) %>%
  arrange(desc(Round1Total))