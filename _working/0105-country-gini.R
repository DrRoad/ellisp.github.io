
library(tidyverse)
library(scales)
library(WDI)
library(ineq) # for Gini
library(acid) # for weighted.gini
library(forcats)
library(RColorBrewer)
library(directlabels)
library(ggrepel)
library(testthat)
library(forecastHybrid)

tmp1 <- WDIsearch("GDP per capita")
tmp2 <- WDIsearch("population")
# "NY.GDP.PCAP.PP.KD"    "GDP per capita, PPP (constant 2005 international $)"  
# - this one sounds and looks like what Milanovic is usign but only goes to 1990
# In http://databank.worldbank.org/data/download/WDIrevisions.xls,
# it is stated that from May 2014 PPP data are onlyu provided back to 1990 due
# to concerns about the risk of inaccuracy the further back from the benchmark year

# alternatives:
# "NY.GDP.PCAP.KD"       "GDP per capita (constant 2000 US$)" 
# "GDP.PC.KD"              "GDP per Capita, constant US$, millions" - doesn't seem to exist
# "NY.GDP.PCAP.CD"       "GDP per capita (current US$)"   
#  "NY.GDP.PCAP.KN"       "GDP per capita (constant LCU)"  
# SPPOPTOTL Population, millions


#==============clean version, 2005===========
gini_palette <- brewer.pal(3, "Set2")
names(gini_palette)[c(1,2)] <- c("Unweighted", "Weighted")

gdp_ppp <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", "SP.POP.TOTL"), start = 1960, end = 2018, extra = TRUE)
gdp_constant <-  WDI(indicator = c("NY.GDP.PCAP.KD", "SP.POP.TOTL"), start = 1960, end = 2018, extra = TRUE)

country_codes <- gdp_ppp %>%
  dplyr::select(iso2c, country) %>%
  distinct()
# everything begining with X except XK (Kosovo) is a grouping eg "Euro area"
# everything with a number in it is a a group eg T5 is South Asia (IDA and IBRD)
# ZF, ZG, ZQ, ZT, ZJ are regions
# OE is the OECD
# EU is the European Union

codes <- country_codes$iso2c
exes <- codes[grepl("^X", codes)]
exes <- exes[exes != "XK"]

regs <- c(exes, codes[grepl("[0-9]", codes)], "ZF", "ZG", "ZQ", "ZT", "ZJ", "OE", "EU")




tmp <- gdp_constant %>%
  rename(gdp = NY.GDP.PCAP.KD) %>%
  mutate(var = "USD exchange rates")

svg("../img/0105-gini-usd.svg", 7, 4)
gdp_ppp %>%
  rename(gdp = NY.GDP.PCAP.PP.KD) %>%
  mutate(var = "Purchasing Power Parity") %>%
  rbind(tmp) %>%
  filter(!iso2c %in% regs) %>%
  filter(!is.na(gdp)) %>%
  group_by(year, var) %>%
  summarise(Unweighted = Gini(gdp),
            Weighted = weighted.gini(gdp, SP.POP.TOTL)$bcwGini) %>%
  gather(variable, value, -year, -var) %>%
  mutate(variable = fct_reorder(variable, -value, fun = last)) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  facet_wrap(~var) +
  geom_line() +
  theme(legend.position = "right") +
  labs(colour = "",
       y = "Gini or weighted Gini") +
  ggtitle("Inter-country inequality over time",
          "GDP per capita, two different methods of comparing across countries
Excludes some countries eg Soviet Union ('Russian Federation' enters series in 1990).") +
  scale_x_continuous("", limits = c(1960, 2020)) +
  scale_colour_manual(values = gini_palette)


#============time series charts=================
p <- gdp_constant %>%
  rename(gdp = NY.GDP.PCAP.KD)  %>%
  # filter(iso2c %in% regs) %>%
  filter(country %in% c("United States", "United Kingdom", "France", "China", "India", "Thailand", "South Africa")) %>%
  ggplot(aes(x = year, y = gdp, colour = country)) +
  geom_line() +
  scale_y_log10("GDP per capita, constant US dollars", label = dollar) +
  labs(x = "", caption = "Source: World Development Indicators") +
  ggtitle("GDP per capita over time, selected countries")

direct.label(p)


#====================scatter plots========
# Figure 4.3
head(gdp_constant)
table(gdp_constant$region)
table(gdp_constant$income)

gdp_summary <- gdp_constant %>%
  filter(!iso2c %in% regs & year >= 1970) %>%
  rename(gdp = NY.GDP.PCAP.KD,
         pop = SP.POP.TOTL) %>%
  arrange(year) %>%
  mutate(country_type = ifelse(income == "High income: OECD", "Western", "Other"),
         country_type = ifelse(region == "East Asia & Pacific (all income levels)" | 
                                 country %in% c("India", "Bangladesh", "Nepal", "Pakistan"), 
                               "Asian", country_type)) %>%
  group_by(iso2c, country, country_type) %>%
  summarise(gdp1970 = gdp[year == 1970],
            growth = (gdp[year == 2013] / gdp1970) ^ (1 / 33) - 1,
            pop1970 = pop[year == 1970]) %>%
  filter(!is.na(growth))  %>%
  mutate(plot_label = ifelse(country_type == "Other", 
                                    "Excluding Asian and Western countries", 
                                    "Asian and Western countries"))

gdp_summary$plot_label <- relevel(factor(gdp_summary$plot_label), "Excluding Asian and Western countries")
gdp_summary$country_type <- fct_relevel(factor(gdp_summary$country_type), c("Asian", "Western"))

p1 <- gdp_summary %>%
  ggplot(aes(x =  gdp1970, y = growth)) +
  facet_wrap(~plot_label) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", linetype = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey40", aes(weight = pop1970)) +
  geom_point(aes(colour = country_type, size = pop1970 / 10^6), alpha = 0.5) +
  geom_point(aes(size = pop1970 / 10^6), colour = "black", shape = 1) +
  # next line gives labels for points but horribly cluttered plot
  # geom_text_repel(aes(label = iso2c)) +
  scale_x_log10("\nGDP per capita in 1970\n\nDotted line is unweighted regression; solid line is population-weighted regression.\n", 
                label = dollar, breaks = c(1000, 5000, 20000)) +
  scale_y_continuous("Average growth between 1970 and 2013", label = percent) +
  scale_size_area("Population\nin 1970 (m)", max_size = 10) +
  labs(colour = "Region",
       caption = "Source: World Development Indicators") +
  ggtitle("GDP in 1970 and subsequent growth",
          "Constant prices, US dollars") +
  theme(legend.position = "right")

p1
# Note the pattern is the same as in Milanovich's graphic, even though 
# the growth rates seem different

#================Figure 4.2====================

# excluding Vietnam because they don't have any data now in WDI until the 1980s
# adding in some other countries by population - not clear why excluded from the original
emerging <- c("India", "Brazil", "Indonesia", "South Africa", "Pakistan", "Bangladesh", "Nigeria",
              "Mexico", "Thailand", "Philippines", "Egypt, Arab Rep.", "Turkey", "Korea, Rep.",
              "Iran, Islamic Rep.", "Myanmar", "Colombia", "Congo, Dem. Rep.")

expect_equal(sum(!emerging %in% gdp_constant$country), 0)

gdp_constant %>% 
  rename(gdp = NY.GDP.PCAP.KD,
         pop = SP.POP.TOTL) %>%
  filter(country %in% c("European Union", "United States", "Japan", emerging)) %>%
  mutate(type = ifelse(country %in% emerging, "emerging", "advanced")) %>%
  group_by(year, type) %>%
  # needs to be false so bars don't draw when not present
  summarise(gdp = sum(gdp * pop, na.rm = FALSE)) %>%
  group_by(type) %>%
  mutate(diffgdp = c(NA, diff(gdp)),
         growth = diffgdp / (gdp - diffgdp)) %>%
  dplyr::select(year, type, growth) %>%
  spread(type, growth) %>%
  mutate(difference = emerging - advanced) %>%  
  ggplot(aes(x = year, weight = difference)) +
  geom_bar() +
  scale_y_continuous("Difference in growth rate between emerging and 
advanced economies, in percentage points",
                     label = percent)
  
# This is much more positive in the late 1960s and 1970s than in the original,
# and perhaps this is all because he had Vietnam (during the war...) and I don't
# have data.

convert_pngs("0105")



#================forecasts of global gini=============
# this is only a taster for what is described in Excursus 4.1 in Milanovic's book
# because it only deals with inter-national inequality, and uses very crude forecasts!
# but it comes up with a modestly similar result.  I see a drop in inter-national inequaltiy
# of about 0.07 whereas they find a drop in global inequality of 0.04.  These feel quite
# potentially consistent (because most forecasts for intranational inequaltiy are increases).

# could do the forecasts of Gini by forecasting GDP per capita, and using the population 
# forecasts from https://esa.un.org/unpd/wpp/Download/Standard/Population/
# but.... effort!...
# so let's just do our own forecasts

#' Forecast gdp and pop for one country gdp_constant
#' and combines it with the historical data
#' context-specific function
forecast_gdp_pop <- function(the_country){
  print(the_country) # for debugging
  # assumes presence in environment of a data.frame called gdp_constant
  country_vars <- gdp_constant %>%
    rename(gdp = NY.GDP.PCAP.KD,
           pop = SP.POP.TOTL) %>%
    arrange(year) %>%
    filter(country == the_country) %>%
    dplyr::select(gdp, pop) %>%
    ts(start = 1960) 

  # a bit of fiddliness is needed because some countries are missing early GDP per person data.
  # maybe they should be knocked out altogether of course... but let's decide that
  # later and explicitly, this function shoudl be able to handle it either way
  years_orig <- time(country_vars)
  rows_include <- which(!is.na(country_vars[ , "gdp"]))
  # some "countries" have virtually no observations, so don't even try to forecast them...
  if(length(rows_include) > 5){ 
    
    country_vars <- country_vars[rows_include, ]
  
    mod_gdp <- hybridModel(country_vars[ , "gdp"], lambda = 0, models = "ae")
    mod_pop <- hybridModel(country_vars[ , "pop"], lambda = 0, models = "ae")
    
    fc_gdp <- forecast(mod_gdp, h = 20)
    fc_pop <- forecast(mod_pop, h = 20)
    
    years <- years_orig[rows_include]
    years <- c(years, max(years + 1):(max(years) + 20))
    
    
    tmp <- data.frame(
      country = the_country,
      gdp_pp = c(fc_gdp$x, fc_gdp$mean),
      pop = c(fc_pop$x, fc_pop$mean),
      year = years
    )} else {
      tmp <- NULL
    }
  return(tmp)
}


# test cases
forecast_gdp_pop("Australia") %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line()

forecast_gdp_pop("China") %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line()


forecast_gdp_pop("Vietnam") %>%
  gather(variable, value, -year, -country) %>%
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  geom_line()

all_countries <- unique(gdp_constant$country)
all_forecasts <- lapply(all_countries, forecast_gdp_pop)

all_forecasts_df <- do.call("rbind", all_forecasts)
head(all_forecasts_df)

all_forecasts_df %>%
  ggplot(aes(x = year, y = gdp_pp, colour = country)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_y_log10()


# need to join back up with iso2c....
all_country_codes <- gdp_constant %>%
  dplyr::select(iso2c, country) %>%
  distinct()

all_forecasts_df %>%
  left_join(all_country_codes, by = "country") %>%
  filter(!iso2c %in% regs) %>%
  group_by(year) %>%
  summarise(Unweighted = Gini(gdp_pp),
            Weighted = weighted.gini(gdp_pp, pop)$bcwGini) %>%
  gather(variable, value, -year) %>%
  mutate(variable = fct_reorder(variable, -value, fun = last)) %>% View
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line() +
  theme(legend.position = "right") +
  labs(colour = "",
       y = "Gini or weighted Gini") +
  ggtitle("Inter-country inequality over time, including crude forecasts",
          "GDP per capita, two different methods of comparing across countries
Excludes some countries eg Soviet Union ('Russian Federation' enters series in 1990).") +
  scale_colour_manual(values = gini_palette)
