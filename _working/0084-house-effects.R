
# See http://m.nzherald.co.nz/nz/news/article.cfm?c_id=1&objectid=11328981 for
# commentary on who did best (Digipoll - who aren't in for 2017)

library(nzelect)
library(mgcv)
library(tidyverse)
library(scales)
library(magrittr)
library(forcats)
library(RColorBrewer)

# TODO - Reid Research, for a smaller group of years and possibly parties

# how many polls per year
polls %>%
   select(ElectionYear, Pollster, MidDate) %>%
   distinct() %>%
   group_by(ElectionYear, Pollster) %>%
   summarise(Polls = n()) %>%
   ungroup() %>%
   mutate(Pollster = fct_reorder(Pollster, Polls)) %>%
   ggplot(aes(x = Polls, y = Pollster, colour = as.factor(ElectionYear))) +
   geom_point() +
   facet_wrap(~ElectionYear)
   


elect_years <- c(2005, 2008, 2011, 2014)
pollsters <- c("Colmar Brunton", "Roy Morgan")
houses <- expand.grid(elect_years, pollsters)
names(houses) <- c("ElectionYear", "Pollster")

parties <- polls %>%
   filter(ElectionYear == 2017) %>%
   distinct(Party) %$%
   Party
parties <- parties[!parties %in% c("Destiny", "Progressive", "Mana", "Conservative")]

for(j in 1:length(parties)){
   the_party = parties[j]
   
   states <- polls %>%
      filter(Pollster == "Election result")  %>%
      filter(Party == the_party) %>%
      slice(-1) # first year election result didn't have any predictors
   
   
   for(i in 1:length(elect_years)){
      
      thedata <- polls %>%
         filter(Pollster %in% houses$Pollster) %>%
         filter(ElectionYear == elect_years[i] & Pollster != "Election result") %>%
         filter(Party == the_party)
      
      mod <- gam(VotingIntention ~ s(as.numeric(MidDate)) + Pollster, 
                 family = "quasibinomial", data = thedata)
      
      preddata <- data.frame(MidDate = as.numeric(states[i, "MidDate"]), Pollster = pollsters)
      
      # house effect is shown by the amount the predicted value from polling
      # is *more* than the actual vote.  So a positive score means the poll
      # overestimated the actual vote:
      houseeffects <- predict(mod, newdata = preddata, type = "response") -
         states[i, "VotingIntention"]
      houses[houses$ElectionYear == elect_years[i], the_party] <- houseeffects
   }

}   

house_colours <- brewer.pal(3, "Set1")
   
names(house_colours) <-   c("Election result", "Colmar Brunton", "Roy Morgan")

svg("../img/0084-house1.svg", 8, 5)
houses %>%
   gather(Party, `Polling overestimate`, -ElectionYear, -Pollster) %>%
   ggplot(aes(x = ElectionYear, y = `Polling overestimate`, colour = Pollster)) +
   geom_hline(yintercept = 0, colour = "black") +
   geom_point() +
   geom_line() +
   facet_wrap(~Party, ncol = 4) +
   scale_colour_manual(values = house_colours) +
   scale_x_continuous("Election year", breaks = c(2005, 2008, 2011, 2014)) +
   theme(legend.position = c(0.9, 0.15))
dev.off()

# probably won't use this one:
houses %>%
   gather(Party, `Polling overestimate`, -ElectionYear, -Pollster) %>%
   ggplot(aes(x = ElectionYear, y = `Polling overestimate`, colour = Party)) +
   geom_hline(yintercept = 0, colour = "black") +
   geom_point(size = 2) +
   geom_line(size = 2) +
   facet_wrap(~Pollster, ncol = 4) +
   scale_colour_manual(values = parties_v)


election_dates <- polls %>%
   filter(Pollster == "Election result") %>%
   select(MidDate) %>%
   distinct()

svg("../img/0084-straight-polls.svg", 12, 7)
polls %>%
   filter(Party %in% parties) %>%
   filter(Pollster %in% c(pollsters, "Election result")) %>%
   mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
          Pollster = fct_relevel(Pollster, "Election result")) %>%
   ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
   geom_vline(xintercept = as.numeric(election_dates$MidDate), colour = "orange") +
   geom_line(alpha = 0.6) +
   scale_y_continuous("Voting intention", label = percent) +
   scale_x_date("") +
   labs( colour = "") +
   facet_wrap(~Party, scales = "free_y")  +
   theme(legend.position = c(0.7, 0.1)) +
   scale_colour_manual(values = house_colours)
dev.off()



#============
predict(mod, newdata = preddata, type = "response", se.fit = TRUE)
   