# Install latest version of nzelect with most recent polling data
# (more up to date than the CRAN version)
devtools::install_github("ellisp/nzelect/pkg1")

library(tidyverse)
library(magrittr)
library(forcats)
library(scales)
library(nzelect)
library(GGally)
library(boot) # for inv.logit
library(mgcv)


#===========house effects on logit scale from previous elections=================
# vector of just the seven main parties with a track record to use
parties <- polls %>%
   filter(ElectionYear == 2017) %>%
   distinct(Party) %>%
   filter(!Party %in% c("Destiny", "Progressive", "Mana", "Conservative")) %$%
   Party

house_bias2 <- function(elect_years, pollsters){
   # Estimates house effects on the *logit* scale.
   # depends on these objects being in environment:
   # polls, parties
   # Note this is different to house_bias() from a previous post,
   # which drew graphics, and estimated bias on the original scale.
   
   houses <- expand.grid(elect_years, pollsters)
   names(houses) <- c("ElectionYear", "Pollster")
   
   for(j in 1:length(parties)){
      the_party = parties[j]
      
      # election results:
      results <- polls %>%
         filter(ElectionYear %in% elect_years & ElectionYear != 2002) %>%
         filter(Pollster == "Election result")  %>%
         filter(Party == the_party) 
      
      
      for(i in 1:length(elect_years)){
         
         # Note we include *all* pollsters in the data for fitting the model
         thedata <- polls %>%
            filter(ElectionYear == elect_years[i] & Pollster != "Election result") %>%
            filter(Party == the_party)
         
         mod <- gam(VotingIntention ~ s(as.numeric(MidDate)) + Pollster, 
                    family = "quasibinomial", data = thedata)
         
         # for predicting values, we only take the pollsters we have an interest in:
         preddata <- data.frame(MidDate = as.numeric(results[i, "MidDate"]), Pollster = pollsters)
         
         # house effect is shown by the amount the predicted value from polling
         # is *more* than the actual vote.  So a positive score means the poll
         # overestimated the actual vote:
         houseeffects <- predict(mod, newdata = preddata, type = "link") -
            logit(results[i, "VotingIntention"])
         houses[houses$ElectionYear == elect_years[i], the_party] <- houseeffects
      }
      
   }   

   houses_av <- houses %>%
      gather(Party, Bias, -ElectionYear, -Pollster) %>%
      group_by(Party, Pollster) %>%
      summarise(Bias = mean(Bias))
   
   return(houses_av)
}

hb1 <- house_bias2(elect_years = c(2005, 2008, 2011, 2014),
                  pollsters   = c("Colmar Brunton", "Roy Morgan"))      

hb2 <- house_bias2(elect_years = c(2011, 2014),
                  pollsters    = c("Reid Research", "Colmar Brunton", "Roy Morgan"))      

house_effects <- hb2 %>%
   filter(Pollster == "Reid Research") %>%
   rbind(hb1) %>%
   arrange(Party, Pollster)

#=========setup======================

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
          VotingIntention = ifelse(VotingIntention < 0.0005, 0.0005, VotingIntention),
          VotingIntention = logit(VotingIntention) - Bias)
   # note that sum(inv.logit(VotingIntention)) now no longer necessarily 
   # adds to 100, because of the bias corrections

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
   scale_y_continuous("logit(Voting Intention), after adjusting for house effects\n",
                      sec.axis = sec_axis(~inv.logit(.) * 100,
                                          name = "Voting Intention, after adjusting for house effects\n")) 

# add a secondary scale

#===========correlations================
polls_w <- PollsElection %>%
   filter(Party %in% parties) %>%
   mutate(PollDate = paste(Pollster, MidDate),
          ID = 1:n()) %>%
   select(Party, VotingIntention, PollDate, MidDate) %>% 
   spread(Party, VotingIntention, fill = 0) %>%
   mutate(MidDate = as.numeric(MidDate))

cors <- cor(polls_w[ , -(1:2)])



# might want to do a graphic here showing the correlations
#===============modelling and predictions==============

names(polls_w) <- make.names(names(polls_w))
mod <- gam(list(
      ACT ~ s(MidDate, k = 3),
      Conservative ~ s(MidDate, k = 3),
      Green ~ s(MidDate),
      Labour ~ s(MidDate),
      Mana ~ s(MidDate, k = 3),
      Maori ~ s(MidDate, k = 3),
      National ~ s(MidDate),
      NZ.First ~ s(MidDate),
      United.Future ~ MidDate
   ),   data = polls_w,   family = mvn(d = 9))

mod_pred <- predict(mod, newdata = electionday, se.fit = TRUE)
summary(mod)
str(mod)
plot(mod, pages = 1, shade = TRUE)

pred_votes <- data.frame(
   Lower = as.vector(mod_pred[["fit"]] -  1.96 * mod_pred[["se.fit"]]),
   Midpoint = as.vector(mod_pred[["fit"]]),
   Upper = as.vector(mod_pred[["fit"]] +  1.96 * mod_pred[["se.fit"]])) %>%
   map_df(function(x){round(inv.logit(x) * 100, 1)}) %>%
   mutate(Party = parties)

pred_votes
#==========simulations============

## estimated cov matrix from model
mod_cov <- solve(crossprod(mod$family$data$R)) 
cov2cor(mod_cov)
cors # tend to be higher than those from the model

se <- as.vector(mod_pred[["se.fit"]])
sims <- inv.logit(MASS::mvrnorm(n = n, 
              mu = mod_pred[["fit"]],
              Sigma = se %*% t(se) * cov2cor(mod_cov))) %>%
   as.data.frame()
names(sims) <- parties
sims_df <- sims %>%
   mutate(ID = 1:n()) %>%
   gather(Party, Vote, -ID) %>%
   group_by(ID) %>%
   mutate(Vote = Vote / sum(Vote))

sims_df %>%
   ggplot(aes(x = Vote)) +
   geom_density() +
   facet_wrap(~Party, scales = "free") +
   scale_x_continuous(label = percent)

#===============simulate electorates================


# probabilities of each seat for Maori, Labour and Mana.  This makes a big difference to 
# the overal results
maori_probs <- c(0.22, 0.7, 0.05)

# see https://en.wikipedia.org/wiki/M%C4%81ori_electorates for the true names of the Maori electorates
electorate_sims <- data_frame(
   orahiu = sample(c("United Future", "Labour"), prob = c(0.6, 0.4), size = n, replace = TRUE),
   epsom = sample(c("ACT", "National", "Labour"), prob = c(0.8, 0.1, 0.1), size = n, replace = TRUE),
   m1 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE),
   m2 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE),
   m3 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE),
   m4 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE),
   m5 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE),
   m6 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE),
   m7 = sample(c("Maori", "Labour", "Mana"), prob = maori_probs, size = n, replace = TRUE)
) %>%
   mutate(sim = 1:n()) %>%
   gather(seat, party, -sim) %>%
   group_by(party, sim) %>%
   summarise(seats = n()) %>%
   spread(party, seats, fill = 0) 

data.frame(party = colnames(electorate_sims)[-1], 
           electorate_seats = as.numeric(electorate_sims[1, -1]))



#============allocate seats==================


seats <- t(sapply(1:n, function(i){
   allocate_seats(votes = as.numeric(sims[i, ]), 
                  electorate = as.numeric(electorate_sims[i, ]))$seats_v
}))

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
