# This script is prep for the shiny app in the ./_working/0108/ folder
# See previous blog posts for sourcing the original data
# App is deployed to https://ellisp.shinyapps.io/nzes2014_x_by_party/ 

library(foreign)
library(tidyverse)
library(forcats)
library(DT)
library(survey)
library(rsconnect)

# Import the New Zealand Election Study data:
nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE, reencode = FALSE)

# full names for all the questions
varlab <- cbind(attributes(nzes_orig)$variable.labels)

# subset which variables we want
vars <- varlab[c(10:22, 94:99, 178:212, 215, 261, 270:288, 308:324, 331, 341:343,
                 351:358, 362:381, 385:403), ] 
vars <- gsub("M.ori", "Māori", vars)

# tidy up the data
nzes <- nzes_orig %>%
  map_df(function(x){
    if(is.factor(x)){
      levels(x) <-  gsub("M.ori", "Māori", levels(x))
      x
    } else {
      x
    }}) %>%
  mutate(partyvote = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
         partyvote = gsub("net.Man", "net-Man", partyvote),
         partyvote = fct_lump(partyvote, 10, other_level = "Another party"),
         partyvote = fct_infreq(partyvote),
         # This magic constant, 3140417 was the size of the electoral roll at the time of election;
         # see http://www.electionresults.govt.nz/electionresults_2014/e9/html/e9_part9_1.html
         dwtfin = dwtfin * 3140.417 / sum(dwtfin) ) 
  nzes <- nzes[ ,c(names(vars), "partyvote", "dwtfin")]

party_numbers <- nzes %>%
  group_by(partyvote) %>%
  summarise(sample_size = n())

nzes <- nzes %>%
  left_join(party_numbers, by = "partyvote") %>%
  mutate(partyvote_n = paste0(partyvote, ", n = ", sample_size),
         partyvote_n = fct_infreq(partyvote_n))

nzes_svy <- svydesign(~1, weights = ~dwtfin, data = nzes)

tmp <- svychisq(~ partyvote + dinterest, design = nzes_svy)
str(tmp)
round(tmp$p.value, 3)

save(vars, file = "0108/vars.rda")
save(nzes, file = "0108/nzes.rda")
save(nzes_svy, file ="0108/nzes_svy.rda")

deployApp("0108", appName = "nzes2014_x_by_party", account = "ellisp")

