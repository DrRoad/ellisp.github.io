

library(survey)
library(tidyverse)
library(data.table)
library(scales)
library(stringr)
library(forcats)
library(ggseas)
library(directlabels)
library(testthat)

#========Download and import data===============
# download survey data from the MBIE (Ministry of Business, Innovation and Employment) website
download.file("http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs/documents-image-library/vw_IVS.zip",
              mode = "wb", destfile = "vw_IVS.zip")

unzip("vw_IVS.zip")

# list of CSV files, each one of which is a copy of a view in the MBIE database:
files <- list.files("IVS")

# import each CSV file as a data.table/data.frame object in RAM with the name of the view
for(i in 1:length(files)){
  print(paste("Importing", files[i]))
  tmp <- fread(paste0("IVS/", files[i]))
  tabname <- str_sub(files[i], end = -5)
  assign(tabname, tmp)
}


#=============Familiarisation============
# First analysis is just to get a feel for the data; not used in the final report
head(vw_IVSSurveyMainHeader)

# total spend by purpose of visit
spend_pov <- vw_IVSSurveyMainHeader %>%
  group_by(Qtr, POV) %>%
  summarise(total_spend = sum(WeightedSpend * PopulationWeight),
            visitors = sum(PopulationWeight),
            sample_size = n())

# graphic  
spend_pov %>%
  # convert the "1997 1" format into a number:
  mutate(qtr = as.numeric(str_sub(Qtr, start = -1)),
         yr = as.numeric(str_sub(Qtr, end = 4)),
         yr_qtr = yr + (qtr - 0.5) / 4) %>%
  # reshape:
  ungroup() %>%
  select(yr_qtr, POV, total_spend, visitors) %>%
  gather(variable, value, -yr_qtr, -POV) %>%
  mutate(POV = fct_reorder(POV, -value)) %>%
  ggplot(aes(x = yr_qtr, y = value, colour = POV)) +
  facet_wrap(~variable, scale = "free_y") +
  geom_line() +
  geom_smooth(se = FALSE) +
  scale_y_continuous("", label = comma) +
  ggtitle("Weighted spend and visitor numbers, visits to New Zealand",
          "Highly seasonal quarterly data") +
  labs(x = "", colour = "Purpose\nof Visit",
       caption = "Source: MBIE International Visitor Survey")


#================Activities=================
head(vw_IVSActivities)
# hmm, an interesting set of "Activities" but we have no way of identifying which are "good" or not
# so we will just count them per person/visit

# number of activities per person
act_pp <- vw_IVSActivities %>%
  group_by(SurveyResponseID) %>%
  summarise(number_activities = n())


#================Queenstown=====================
head(vw_IVSItineraryPlaces)
# 514 distinct places listed:
sort(unique(vw_IVSItineraryPlaces$WhereStayed))

places_visited <- vw_IVSItineraryPlaces %>%
  group_by(SurveyResponseID) %>%
  summarise(number_places_visited = length(unique(WhereStayed)),
            visited_queenstown = ifelse("Queenstown" %in% unique(WhereStayed), 
                                        "Visited Queenstown", "Did not visit Queenstown"))

qt_visitors <- places_visited %>%
  right_join(vw_IVSSurveyMainHeader, by = "SurveyResponseID") %>%
  mutate(qtr = as.numeric(str_sub(Qtr, start = -1)),
         yr = as.numeric(str_sub(Qtr, end = 4)),
         yr_qtr = yr + (qtr - 0.5) / 4,
         visited_queenstown = ifelse(is.na(visited_queenstown), "Did not visit *anywhere*", visited_queenstown)) %>%
  left_join(act_pp, by = "SurveyResponseID") %>%
  group_by(visited_queenstown, yr_qtr)

expect_equal(nrow(qt_visitors), nrow(vw_IVSSurveyMainHeader))

# Two variables look to be about length of stay; but NoDaysInNZ has 120,000+ NA values so we will use the other
# option which has only 196
summary(vw_IVSSurveyMainHeader[ , c("NoDaysInNZ", "LengthOfStay")])

qt_visitors_sum <- qt_visitors %>%
  summarise(total_spend = sum(WeightedSpend * PopulationWeight),
            total_days = sum(LengthOfStay * PopulationWeight, na.rm = TRUE),
            total_activities = sum(number_activities * PopulationWeight, na.rm = TRUE),
            visitors = sum(PopulationWeight),
            spend_per_visitor = total_spend / visitors,
            days_per_visitor = total_days / visitors,
            activities_per_visitor = total_activities / visitors,
            sample_size = n()) %>%
  ungroup() %>%
  mutate(visited_queenstown = fct_reorder(visited_queenstown, total_spend))
  

palette <- c("Visited Queenstown" = "red", "Did not visit Queenstown" = "blue", "Did not visit *anywhere*" = "grey")

# do they spend more per visit?

qt_visitors_sum %>%
  ggplot(aes(x = yr_qtr, y = spend_per_visitor, colour = visited_queenstown)) +
  geom_line(alpha = 0.15) +
  stat_stl(s.window = 7, size = 1.2) +
  theme(legend.position = "right") +
  scale_colour_manual("", values = palette) +
  scale_y_continuous("Spend per visitor (seasonally adjusted)", label = dollar) +
  labs(x =  "", caption = "Source: MBIE International Visitor Survey")

# do they stay more days and do more activities?
qt_visitors_sum %>%
  select(visited_queenstown, yr_qtr, days_per_visitor, activities_per_visitor) %>%
  gather(variable, value, -visited_queenstown, -yr_qtr) %>%
  ggplot(aes(x = yr_qtr, y = value, colour = visited_queenstown)) +
  facet_wrap(~variable, scales = "free_y") +
  geom_line(alpha = 0.15) +
  stat_stl(s.window = 7, size = 1.2) +
  scale_colour_manual("", values = palette) +
  scale_y_continuous("Value (seasonally adjusted)", label = comma) +
  labs(x =  "", caption = "Source: MBIE International Visitor Survey")

# There is a problem here that if you choose *any* destination, people who visited there
# do more activites and stayed longer in NZ than average.  Why?  Probably because indicating
# *any* itinerary point means they were more engaged (either with NZ, or with the country...)
