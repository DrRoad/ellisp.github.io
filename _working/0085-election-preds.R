library(tidyverse)
library(forcats)
library(scales)
library(nzelect)
library(GGally)
library(boot) # for inv.logit
library(mgcv)



#=========setup======================
load("../data/house_effects.rda")

ThisElection <- "2017-09-23"

electionday <- data_frame(
   MidDate = as.numeric(as.Date(ThisElection))
)

PollsElection <- polls %>%
   as_tibble() %>%
   filter(ElectionYear == substring(ThisElection, 1, 4)) %>%
   left_join(house_effects, by = c("Pollster", "Party")) %>%
   # we don't have bias estimates for Mana or Conservative parties,
   # or for pollsters other than Colmar Brunton, Reid Research and 
   # Roy Morgan.  So replace NA in bias with zero:
   mutate(Bias = ifelse(is.na(Bias), 0, Bias),
          VotingIntention = VotingIntention - Bias,
          VotingIntention = ifelse(VotingIntention < 0, 0, VotingIntention))
   # note that VotingIntention now no longer necessarily adds to 100

parties <- unique(PollsElection$Party)
parties <- sort(as.character(parties[!parties %in% c("Destiny", "Progressive")]))
   
n <- 1000

PollsElection %>%
   filter(Party %in% parties) %>%
   mutate(Party = fct_drop(Party)) %>%
#   mutate(Party = fct_reorder(Party, VotingIntention, fun = max)) %>%
   ggplot(aes(x = MidDate, y = VotingIntention)) +
   geom_point(aes(colour = Pollster)) +
   geom_line(aes(colour = Pollster)) +
   geom_smooth(se = FALSE) +
   facet_wrap(~Party, scales = "free_y") +
   scale_y_continuous(label = percent) +
   labs(y = "Voting intention after adjusting for house effects")

#===========correlations================
polls_w <- PollsElection %>%
   filter(Party %in% parties) %>%
   mutate(PollDate = paste(Pollster, MidDate),
          ID = 1:n()) %>%
   select(Party, VotingIntention, PollDate) %>% 
   spread(Party, VotingIntention, fill = 0)

cors <- cor(polls_w[ , -1])

# might want to do a graphic here showing the correlations
#===============modelling and predictions==============
modresults <- matrix(0, nrow = length(parties), ncol = 2)
rownames(modresults) <- parties
colnames(modresults) <- c("prediction", "se")

for(i in 1:length(parties)){
   theparty <- parties[i]
   
   thedata <- PollsElection %>%
      filter(Party == theparty)
   
   if(theparty != "United Future"){
      mod <- gam(VotingIntention ~ s(as.numeric(MidDate)),
                 family = "quasibinomial", data = thedata)
   } else {
      # United Future gives wildly unrealistic predictions, including
      # a small chance of 75% of electorate voting for them.  Because
      # not enough data.  So shrink down model to a simpler one.
      mod <- gam(VotingIntention ~ as.numeric(MidDate),
                 family = "quasibinomial", data = thedata)
   }
   
   pred <- predict(mod, newdata = electionday, se.fit = TRUE)
   modresults[i , ] <- c(pred$fit, pred$se.fit)
}

pred_sims <- as.data.frame(round(inv.logit(cbind(
   modresults[, "prediction"] -  1.96 * modresults[ , "se"],
   modresults[, "prediction"],
   modresults[, "prediction"] +  1.96 * modresults[ , "se"])) 
   * 100, 1))
names(pred_sims)  <- c("Lower", "Midpoint", "Upper")
pred_sims
#==========simulations============


se <- modresults[ , "se"]
sims <- inv.logit(MASS::mvrnorm(n = n, 
              mu = modresults[, "prediction"],
              Sigma = se %*% t(se) * cors))

sims_df <- sims %>%
   as.data.frame() %>%
   mutate(ID = 1:n()) %>%
   gather(Party, Vote, -ID) %>%
   group_by(ID) %>%
   mutate(Vote = Vote / sum(Vote))

sims_df %>%
   ggplot(aes(x = Vote)) +
   geom_density() +
   facet_wrap(~Party, scales = "free") +
   scale_x_continuous(label = percent)

# next bit should be simulated too
electorates <- c(1,0,0,1,0,1,1,1,1)

seats <- as.data.frame(t(apply(sims, 1, function(x){
   allocate_seats(x, electorate = electorates)$seats_v
})))

names(seats) <- gsub("M.ori", "Maori", names(seats))
names(seats) <- gsub("NZ First", "NZ_First", names(seats))

seats <- seats %>%
   mutate(NatCoal = ACT + Conservative + National + `United Future` + Maori,
          LabCoal = Labour + Green + Mana + NZ_First)


#==================presentation=====================
seats %>%
   dplyr::select(NatCoal, LabCoal) %>%
   gather(Coalition, Seats) %>%
   ggplot(aes(x = Seats, colour = Coalition)) +
   geom_density() 

seats %>%
   mutate(NatMaj = NatCoal - LabCoal) %>%
   ggplot(aes(x = NatMaj)) +
   geom_density() 

# This graph is interestin.
# note that the correlations here are much further from zero than the 
# correlations of votes, because of the seats algorithm
seats %>%
   mutate(Other = ACT + `United Future` + Conservative + Mana + Maori) %>%
   dplyr::select(Green, Labour, National, NZ_First, Other) %>%
   ggpairs() +
   ggtitle(paste("Possible outcomes for number of seats", ThisElection))

# instead of assuming NZFirst go with Labour, make scenarios called
# "NZFirst decides"
seats %>%
   summarise(NatCoalWin = mean(NatCoal > LabCoal),
             LabCoalWin = mean(LabCoal > NatCoal),
             NatAloneWin = mean(National > 60),
             LabGrnWin = mean((Green + Labour) > 60),
             TieCoalitions = mean(NatCoal == LabCoal))
