library(foreign)
library(tidyverse)
library(forcats)
library(DT)
library(survey)

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE, reencode = FALSE)

varlab <- cbind(attributes(nzes_orig)$variable.labels)

nzes <- nzes_orig %>%
    # party vote:
  mutate(dpartyvote = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
         dpartyvote = gsub("M.ori", "Māori", dpartyvote),
         dpartyvote = gsub("net.Man", "net-Man", dpartyvote),
         dpartyvote = fct_infreq(dpartyvote),
         dwtfin = dwtfin * 3140.417 / sum(dwtfin) )  # 3140417 was the size of the electoral roll at the time of election

vars <- varlab[c(10:22, 94:99, 178:212, 215, 261, 270:288, 308:324, 331, 341:343,
                   351:358, 362:381, 385:403, 422, 424), ]



nzes_svy <- svydesign(~1, weights = ~dwtfin, data = nzes)

svytable(~dpartyvote + dmarital, nzes_svy)



?svytable






save(vars, file = "0108/vars.rda")
save(nzes, file = "0108/nzes.rda")

table(nzes$dpartyvote)
