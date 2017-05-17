library(tidyverse)
library(maptools)

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

ggplot(PrecinctData, aes(x = p_over, y = p_under, col = contest)) +
  geom_point()

SmallerData <- data %>%
  filter(!(contest %in% c("Board of Supervisors, District 3",
                          "Board of Supervisors, District 5"))) %>%
  group_by(precinct) %>%
  summarise(p_over = mean(over),
            p_under = mean(under),
            contest = unique(contest))

ggplot(SmallerData, aes(x = p_over, y = p_under, col = contest)) +
  geom_point() +
  geom_smooth(method = lm, se = F, aes(col = contest))






Districts <- readOGR(dsn = "./data/2012lines/", layer = "SF_BOS_20120702_nowater")
hello <- fortify(Districts)

ggplot(hello, aes(x=long, y=lat, group=group)) + 
  geom_polygon() +
  theme_void()

out  = NULL
for (j in c(1, 3, 5, 7, 9, 11)) {
  assign(paste0("District", j),
         UsefulImage %>%
           filter(contest == paste("Board of Supervisors, District", j, sep = " ")))
  
  
  # Could do another loop here to get data on every district, w/ something like:
  # paste("round", i, j, sep = ".") and above for loop
  
  assign(paste("round", "0", j, sep = "."), data.frame(unique(get(paste0("District", j))[ ,4])))
  assign(colnames(get(paste("round", "0", j, sep = "."))), c("candidate"))
  assign(paste("loser", "0", j, sep = "."), data.frame(candidate = character()))
  
  assign(paste0("a",j), (nrow(get(paste("round", "0", j, sep = "."))) - 2))
  c <- data.frame(get(paste0("a",j)))
  out=rbind(out,c)
}

ContestData <- data %>%
  group_by(contest) %>%
  summarise(p_over = mean(over),
            p_under = mean(under))

ContestData <- cbind(ContestData, out) %>%
  rename(rounds = get.paste0..a...j..) %>%
  gather(key = error, value = prop, p_over, p_under)

ggplot(ContestData, aes(x = rounds, y = prop, color = error)) +
  geom_point()