
# Install the latest version of the nzcensus package if not already installed:
# devtools::install_github("ellisp/nzelect/pkg2")

library(recmap)
library(nzcensus)
library(tidyverse)
library(viridis)


colour_scale <- function(x, palette = viridis(100)){
   levs <- round(x / max(x, na.rm = TRUE) * length(palette))
   return(as.character(palette[levs]))
}
#x <- comb_data$PropUnemploymentBenefit2013
make_legend <- function(x, palette = viridis(100), title = NULL, 
                        location = "right", 
                        multiplier = 100, digits = 1, ...){
   y <- seq(from= min(x), to = max(x), length.out = 5)
   levsy <- round(y / max(x, na.rm = TRUE) * length(palette))
   legend(location, legend = round(y * multiplier, digits), 
          pch = 15, col = palette[levsy], text.col = palette[levsy],
          bty = "n", title = title, ...)
   title(xlab = "Source: Statistics New Zealand Census 2013, in nzcensus R package",
         adj = 1, col.lab = "grey50", cex.lab = 0.8)
   
}

#=========rectangle cartogram==========
tmp <- with(filter(REGC2013, !grepl("Area Outside", REGC2013_N)), 
            data.frame(x = WGS84Longitude,
                       y = WGS84Latitude,
                       dx = 12,
                       dy = 8,
                       z = ResidentPop2013,
                       name = gsub(" Region", "", as.character(REGC2013_N)),
                       value = PropUnemploymentBenefit2013,
                       stringsAsFactors = FALSE)) %>%
   mutate(colour = colour_scale(value))

svg("../img/0094-rect.svg", 8, 8)
par(family = "myfont", font.main= 1)
tmp %>%
   recmap() %>%
   plot(col.text = "grey10", col = tmp[, "colour"], border = "white")
   title(main = "Unemployment by region; regions sized by usual resident population")
make_legend(tmp$value, title = "Proportion of all individuals\non unemployment benefit",
            location = "left", cex = 0.8)
dev.off()

#===============shape-preserving cartogram=============
# change to "Name" when have new version
comb_data <- reg_cart_simpl@data %>%
   left_join(REGC2013, by = c("Name" = "REGC2013_N")) 

svg("../img/0094-reg-cart.svg", 8, 7)
   par(family = "myfont", font.main= 1, fg = "grey75")
   plot(reg_cart_simpl,
     col = colour_scale(comb_data$PropUnemploymentBenefit2013))
   title(main = "Unemployment by region; regions sized by usual resident population")
   make_legend(comb_data$PropUnemploymentBenefit2013, 
               title = "Proportion of all individuals\non unemployment benefit",
               location = "left", cex = 0.8)
dev.off()

svg("../img/0094-reg-cart-2.svg", 8, 7)
par(family = "myfont", font.main= 1, fg = "grey75")
plot(reg_cart_simpl,
     col = colour_scale(comb_data$PropSmoker2013))
title(main = "Smokers by region; regions sized by usual resident population")
make_legend(comb_data$PropSmoker2013, 
            title = "Percentage of all individuals\nwho smoke",
            location = "left", cex = 0.8)
dev.off()


svg("../img/0094-reg-cart-3.svg", 8, 7)
par(family = "myfont", font.main= 1, fg = "grey75")
plot(reg_cart_simpl,
     col = colour_scale(comb_data$MedianRentHH2013))
title(main = "Median rent; regions sized by usual resident population")
make_legend(comb_data$MedianRentHH2013, 
            title = "Median rent",
            location = "left", cex = 0.8, 
            multiplier = 1, digits = 0)
dev.off()

#==========prepare for shiny app==============

save(comb_data, file = "0094-cartograms/comb_data.rda")
variables <- names(comb_data)[grepl("^Prop", names(comb_data))]
variables <- gsub("^Prop", "", variables)
variables <- gsub("2013", "", variables)
save(variables, file = "0094-cartograms/variables.rda")
save(reg_cart_simpl, file = "0094-cartograms/reg_cart_simpl.rda")
