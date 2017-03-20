
# See http://m.nzherald.co.nz/nz/news/article.cfm?c_id=1&objectid=11328981 for
# commentary on who did best (Digipoll - who aren't in for 2017)

library(nzelect)
library(mgcv)
library(tidyverse)
library(scales)
library(magrittr)
library(forcats)
library(RColorBrewer)



#=====================data prep=======================

house_colours <- brewer.pal(4, "Set1")
names(house_colours) <-   c("Election result", "Reid Research", "Colmar Brunton", "Roy Morgan")

parties <- polls %>%
   filter(ElectionYear == 2017) %>%
   distinct(Party) %>%
   filter(!Party %in% c("Destiny", "Progressive", "Mana", "Conservative")) %$%
   Party


#===============introductory graphics========================
election_dates <- polls %>%
   filter(Pollster == "Election result") %>%
   select(MidDate) %>%
   distinct()

d1 <- polls %>%
   filter(Party %in% parties) %>%
   filter(Pollster %in% c("Reid Research", "Colmar Brunton", "Roy Morgan", "Election result")) %>%
   mutate(Party = fct_reorder(Party, VotingIntention, .desc = TRUE),
          Pollster = fct_relevel(Pollster, "Election result")) 

p1 <- d1 %>%
   ggplot(aes(x = MidDate, y = VotingIntention, colour = Pollster)) +
   geom_vline(xintercept = as.numeric(election_dates$MidDate), colour = "orange") +
   geom_line(alpha = 0.4) +
   geom_smooth(data = filter(d1, Pollster != "Election result"), span = .3, se = FALSE) +
   geom_line(data = filter(d1, Pollster == "Election result"), size = 1.5) +
   scale_y_continuous("Voting intention", label = percent) +
   scale_x_date("") +
   labs( colour = "")   +
   scale_colour_manual(values = house_colours) 

svg("../img/0084-straight-polls-1.svg", 12, 7)
p1 +
   facet_wrap( ~ Party, scales = "free_y") +
   theme(legend.position = c(0.7, 0.1)) 
dev.off()

svg("../img/0084-straight-polls-2.svg", 12, 7)
p1 +
   facet_grid(Pollster ~ Party) +
   theme(legend.position = "none")
dev.off()


#=============estimate and present house "bias"=============

house_bias <- function(elect_years, pollsters){
   # depends on these objects being in environmenet:
   # polls, house_colours, parties
   
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
         houseeffects <- predict(mod, newdata = preddata, type = "response") -
            results[i, "VotingIntention"]
         houses[houses$ElectionYear == elect_years[i], the_party] <- houseeffects
      }
   
   }   
   
   p <- houses %>%
      gather(Party, `Polling overestimate`, -ElectionYear, -Pollster) %>%
      ggplot(aes(x = ElectionYear, y = `Polling overestimate`, colour = Pollster)) +
      geom_hline(yintercept = 0, colour = "black") +
      geom_point() +
      geom_line() +
      facet_wrap(~Party, ncol = 4) +
      scale_colour_manual(values = house_colours) +
      scale_x_continuous("Election year", breaks = c(2005, 2008, 2011, 2014), limits = c(2004, 2015)) +
      scale_y_continuous(label = percent) +
      theme(legend.position = c(0.9, 0.15))
   
   return(p)
}
   
svg("../img/0084-house1.svg", 8, 5)
   house_bias(elect_years = c(2005, 2008, 2011, 2014),
              pollsters   = c("Colmar Brunton", "Roy Morgan"))      
dev.off()

svg("../img/0084-house2.svg", 8, 5)
# Labour Roy MOrgan looks different in this one than the previous:
house_bias(elect_years = c(2011, 2014),
           pollsters    = c("Reid Research", "Colmar Brunton", "Roy Morgan"))      
dev.off()

#===================how many polls per year==========================
svg("../img/0084-polls-year.svg", 8, 7)
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
dev.off()

#============conver to PNGs==============

convert_pngs("0084")
