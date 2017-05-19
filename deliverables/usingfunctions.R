mayorballotimage <- tidy_ballotimage(rawimage = "http://www.acgov.org/rov/rcv/results/230/BerkeleyMayor/ballot_image.txt", 
                                     masterkey = "http://www.acgov.org/rov/rcv/results/230/BerkeleyMayor/master_lookup.txt")

nicemayorimage <- mayorballotimage %>% 
  filter(vote_rank != 4) %>%
  spread(key = vote_rank, value = candidate)

mayorelectionresults <- rcv_results(rcvimage = mayorballotimage, rcvcontest = "Mayor - Berkeley (RCV)")


districtballotimage <- tidy_ballotimage("http://www.sfelections.org/results/20161108/data/20161206/20161206_ballotimage.txt", 
                                        "http://www.sfelections.org/results/20161108/data/20161206/20161206_masterlookup.txt")


district7results <- rcv_results(districtballotimage, "Board of Supervisors, District 7")

rcv_sankey_plot(rcvresults = district7results)
rcv_sankey_plot(mayorelectionresults)


for (j in unique(districtballotimage$contest)) {
  assign(paste0("Election:", j, "Results"), 
         rcv_results(rcvimage = districtballotimage, rcvcontest = j))
}