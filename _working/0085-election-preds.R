library(tidyverse)
library(forcats)
library(scales)
library(nzelect)
library(GGally)
library(boot) # for inv.logit

#=========setup======================
ThisElection <- "2017-09-23"

electionday <- data_frame(
   MidDate = as.numeric(as.Date(ThisElection))
)

PollsElection <- polls %>%
   filter(ElectionYear == substring(ThisElection, 1, 4)) 

parties <- unique(PollsElection$Party)
parties <- sort(as.character(parties[!parties %in% c("Destiny", "Progressive")]))
   
n <- 1000

sims <- as.data.frame(matrix(0, nrow = n, ncol = length(parties)))
names(sims) <- parties


#===============simulations==============

# Thgis method doesn't work well with the correlations.
# eg coirrelation of NZ_First and National is way too strong

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
   print(paste(c(theparty, round(inv.logit(pred$fit + c(-2, 2) * pred$se.fit) * 100, 1)), collapse = " : "))
   sims[ , i] <- inv.logit(rnorm(1000, pred$fit, pred$se.fit))
}


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
   select(NatCoal, LabCoal) %>%
   gather(Coalition, Seats) %>%
   ggplot(aes(x = Seats, colour = Coalition)) +
   geom_density() 

seats %>%
   mutate(NatMaj = NatCoal - LabCoal) %>%
   ggplot(aes(x = NatMaj)) +
   geom_density() 


seats %>%
   mutate(Other = ACT + `United Future` + Conservative + Mana + Maori) %>%
   select(Green, Labour, National, NZ_First, Other) %>%
   ggpairs() +
   ggtitle(paste("Possible outcomes for number of seats", ThisElection))

seats %>%
   summarise(NatCoalWin = mean(NatCoal > LabCoal),
             LabCoalWin = mean(LabCoal > NatCoal),
             NatAloneWin = mean(National > 60),
             LabGrnWin = mean((Green + Labour) > 60),
             TieCoalitions = mean(NatCoal == LabCoal))
