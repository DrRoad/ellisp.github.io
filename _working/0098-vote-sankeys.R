
library(tidyverse)
library(forcats)
library(riverplot)
library(sankeyD3)
library(foreign)
library(testthat)
library(mice)
library(grid)

# caution networkD3 has its own sankeyD3 with less features.  Make sure you know which one you're using

# See previous blog posts for where this comes from:
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")

nzes_orig <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                       to.data.frame = TRUE, trim.factor.names = TRUE)
# Five versions of each row, so when we do imputation later on we
# in effect doing multiple imputation:
nzes <- nzes_orig[rep(1:nrow(nzes_orig), each = 5), ] %>%
   # party vote in 2014:
   mutate(partyvote2014 = ifelse(is.na(dpartyvote), "Did not vote", as.character(dpartyvote)),
          partyvote2014 = gsub("M.ori", "Maori", partyvote2014),
          partyvote2014 = gsub("net.Man", "net-Man", partyvote2014),
          partyvote2014 = fct_infreq(partyvote2014)) %>%
   mutate(partyvote2014 = fct_lump(partyvote2014, 5)) 

actual_vote <- data_frame(
   partyvote2014 = c("National", "Labour", "Green", "NZ First", "Other", "Did not vote"),
   freq = c(1131501, 604534, 257356, 208300, 
            31850 + 16689 + 5286 + 95598 + 34095 + 10961 + 5113 + 1730 + 1096 + 872 + 639,
            NA)
)

# calculate the did not vote, from the 77.9 percent turnout
actual_vote[6, 2] <- (100 / 77.9 - 1) * sum(actual_vote[1:5, 2])

# check I did the turnout sums right:
expect_equal(0.779 * sum(actual_vote[ ,2]), sum(actual_vote[1:5, 2]))

reweight_ratios <- nzes %>%
   group_by(partyvote2014) %>%
   summarise(survey = sum(dwtfin)) %>%
   left_join(actual_vote, by = "partyvote2014") %>%
   mutate(ratio = freq / survey)

nzes2 <- nzes %>%
   left_join(reweight_ratios[, c("partyvote2014", "ratio")]) %>%
   ungroup() %>%
   mutate(weight = dwtfin * ratio) %>%
   mutate(weight = weight / mean(weight)) %>%
   mutate(partyvote2011 = as.factor(ifelse(dlastpvote == "Don't know", NA, as.character(dlastpvote)))) %>%
   # select a smaller number of variables so imputation is feasible:
   select(weight, partyvote2014, partyvote2011, dethnicity_m, dage, dhhincome, dtradeunion, dprofassoc,
          dhousing, dclass, dedcons, dwkpt)

# impute the missing values.  Although we are imputing only a single set of values,
# because we are doing it with each row repeated five times this has the same impact,
# for the simple analysis we do later on, as multiple imputation:
nzes3 <- complete(mice(nzes2, m = 1))

#=======================previous years vote riverplot version============

the_data <- nzes3 %>%
   mutate(
      partyvote2011 = fct_lump(partyvote2011, 5), 
       # add two spaces to ensure the partyvote2011 does not have any
       # names that exactly match the party vote in 2014
       partyvote2011 = paste(partyvote2011, "  "),
       partyvote2011 = gsub("M.ori", "Maori", partyvote2011),
       partyvote2011 = ifelse(grepl("I did not vote", partyvote2011), "Did not vote  ", as.character(partyvote2011))) %>%
   group_by(partyvote2011, partyvote2014) %>%
   summarise(vote_prop = sum(weight)) %>%
   ungroup() 

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
   left_join(nodes_ref[ , c("fullname", "ID")], by = c("col1" = "fullname")) %>%
   rename(N1 = ID) %>%
   left_join(nodes_ref[ , c("fullname", "ID")], by = c("col2" = "fullname")) %>%
   rename(N2 = ID) %>%
   as.data.frame(stringsAsFactors = FALSE)

rp <- makeRiver(nodes = as.vector(nodes_ref$ID), edges = edges,
                node_labels = nodes_ref$fullname,
                # manual vertical positioning by parties.  Look at
                # nodes_ref to see the order in which positions are set:
                # node_ypos = c(1, 2.8, 2, 3.7, 5, 6.2, 7 , 2, 3, 3.9, 5, 6.1),
                node_xpos = nodes_ref$position,
                # set party colours; all based on those in nzelect::parties_v:
                node_styles = list(I = list(col = "#d82a20"), # red labour
                                   J = list(col = "#00529F"), # blue national
                                   K = list(col = "black"),   # black NZFirst
                                   H = list(col = "#098137"), # green
                                   C = list(col = "#d82a20"), # labour
                                   B = list(col = "#098137"), # green
                                   D = list(col = "#00529F"), # national
                                   E = list(col = "black")))  # NZ First

ds <- default.style()
ds$srt <- 0
ds$textcol <- "grey95"

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


#=======================sankeyD3 version====================

nodes_ref2 <- nodes_ref %>%
   mutate(ID = as.numeric(as.factor(ID)) - 1) %>%
   as.data.frame()

edges2 <-    the_data %>%
   ungroup() %>%
   left_join(nodes_ref2[ , c("fullname", "ID")], by = c("col1" = "fullname")) %>%
   rename(N1 = ID) %>%
   left_join(nodes_ref2[ , c("fullname", "ID")], by = c("col2" = "fullname")) %>%
   rename(N2 = ID) %>%
   as.data.frame(stringsAsFactors = FALSE) %>%
   mutate(Value = Value / sum(Value) * 100)



# not sure how .domain([]) works, so have had to set the colours by hand...
# .domain(["Did not vote", "Green",   "Labour",  "National", "NZ First", "Other",   "Labour  ", "Other  ", "Don\'t know  ", "Green  "])


pal <- 'd3.scaleOrdinal()
         .range(["#DCDCDC", "#098137", "#d82a20", "#00529F",  "#000000",  "#DCDCDC", 
                 "#DCDCDC", "#098137", "#d82a20", "#00529F", "#000000", "#DCDCDC"]);'

sankeyNetwork(Links = edges2, Nodes = nodes_ref2, 
              Source = "N1", Target = "N2", Value = "Value",
              NodeID = "fullname",
              NodeGroup = "fullname",
              LinkGroup = "col2",
              fontSize = 12, nodeWidth = 30,
              colourScale = JS(pal),
              numberFormat = JS('d3.format(".1f")'),
              fontFamily = "Calibri", units = "%", 
              nodeShadow = TRUE,
              showNodeValues = FALSE,
              width = 700, height = 500) %>% 
   networkD3::saveNetwork(file = '../img/0098-sankey.html')


