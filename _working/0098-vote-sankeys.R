
library(tidyverse)
library(forcats)
library(riverplot)
library(sankeyD3)
library(foreign)
library(testthat)
library(grid)

# See previous blog posts for where this comes from:
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE)
# party vote:
nzes <- nzes_orig %>%
   mutate(dpartyvote2 = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
          dpartyvote2 = gsub("M.ori", "Maori", dpartyvote2),
          dpartyvote2 = gsub("net.Man", "net-Man", dpartyvote2),
          dpartyvote2 = fct_infreq(dpartyvote2)) %>%
   mutate(dpartyvote2 = fct_lump(dpartyvote2, 5)) 

actual_vote <- data_frame(
   dpartyvote2 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
   freq = c(1131501, 604534, 257356, 208300, 
            31850 + 16689 + 5286 + 95598 + 34095 + 10961 + 5113 + 1730 + 1096 + 872 + 639,
            NA)
)

# calculate the did not vote, from the 77.9 percent turnout
actual_vote[6, 2] <- (100 / 77.9 - 1) * sum(actual_vote[1:5, 2])

# check I did the turnout sums right:
expect_equal(0.779 * sum(actual_vote[ ,2]), sum(actual_vote[1:5, 2]))

reweight_ratios <- nzes %>%
   group_by(dpartyvote2) %>%
   summarise(survey = sum(dwtfin)) %>%
   left_join(actual_vote, by = "dpartyvote2") %>%
   mutate(ratio = freq / survey)

nzes <- nzes %>%
   left_join(reweight_ratios[, c("dpartyvote2", "ratio")]) %>%
   ungroup() %>%
   mutate(weight = dwtfin * ratio) %>%
   mutate(weight = weight / mean(weight)) %>%
   select(-dwtfin, -ratio)

# note in theory should also reweight for 2008 election, and 
# in fact use raking

#=======================previous years vote---------

the_data <- nzes %>%
   mutate(
      partyvote2011 = ifelse(is.na(dlastpvote), "Don't know", as.character(dlastpvote)),
      partyvote2011 = fct_lump(partyvote2011, 6), 
       # add some spaces to ensure the partyvote2011 does not have any
       # names that exactly match the party vote in 2014
       partyvote2011 = paste(partyvote2011, "  "),
       partyvote2011 = gsub("M.ori", "Maori", partyvote2011)) %>%
   
   group_by(partyvote2011, dpartyvote2) %>%
   summarise(vote_prop = sum(weight))

# change names to the names wanted by makeRiver
names(the_data) <- c("col1", "col2", "Value")

# node ID need to be characters I think
c1 <- unique(the_data$col1)
c2 <- unique(the_data$col2)
nodes_ref <- data_frame(fullname = c(c1, c2)) %>%
   mutate(position = rep(c(1, 2), times = c(length(c1), length(c2)))) %>%
   mutate(ID = LETTERS[1:n()])

edges <- 
   the_data %>%
   ungroup() %>%
   left_join(nodes_ref, by = c("col1" = "fullname")) %>%
   rename(N1 = ID) %>%
   left_join(nodes_ref, by = c("col2" = "fullname")) %>%
   rename(N2 = ID) %>%
   as.data.frame(stringsAsFactors = FALSE)

rp <- makeRiver(nodes = as.vector(nodes_ref$ID), edges = edges,
                node_labels = nodes_ref$fullname,
                # manual vertical positioning by parties.  Look at
                # nodes_ref to see the order in which positions are set:
                node_ypos = c(1, 2.8, 2, 3.7, 5, 6.2, 7 , 2, 3, 3.9, 5, 6.1, 7),
                node_xpos = nodes_ref$position,
                # set party colours; all based on those in nzelect::parties_v:
                node_styles = list(J = list(col = "#d82a20"), # red labour
                                   K = list(col = "#00529F"), # blue national
                                   L = list(col = "black"),   # black NZFirst
                                   I = list(col = "#098137"), # green
                                   D = list(col = "#d82a20"),
                                   B = list(col = "#098137"),
                                   E = list(col = "#00529F"),
                                   F = list(col = "black")))

ds <- default.style()
ds$srt <- 0
ds$textcol <- "grey95"

showtext.auto(enable = TRUE)

mygp <- gpar(fontfamily = "myfont", col = "grey75")
# using PNG rather than SVG as vertical lines appear in the SVG version

png("../img/0098-vote.png", 8 * 600, 6 * 600, res = 600)
par(bg = "grey40", family = "myfont")

# note the plot_area argument - for some reason, defaults to using only half the
# vertical space available, so set this to higher than 0.5!:
plot(rp, default_style = ds, plot_area = 0.9)

title(main = "Self-reported party vote in 2011 compared to 2014", 
      col.main = "white", font.main = 1)

grid.text(x = 0.15, y = 0.1, label = "2011 party vote", gp = mygp)
grid.text(x = 0.85, y = 0.1, label = "2014 party vote", gp = mygp)
grid.text(x = 0.95, y = 0.03, 
          gp = gpar(fontfamily = "myfont", fontsize = 7, col = "grey75"), just = "right",
          label = "Source: New Zealand Election Study, analysed at https://ellisp.github.io")
dev.off()


