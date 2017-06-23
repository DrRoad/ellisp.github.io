library(tidyverse)
library(pscl)
library(forcats)

#=========2004 election to 2007 election==============
data(AustralianElectionPolling)
data(AustralianElections)

tail(AustralianElections)

days_between_elections <- as.integer(diff(as.Date(c("2004-10-09", "2007-11-24")))) + 1
# ALP vote (as per Jackman's book)


#----------------no polls inbetween the elections------------
d1 <- list(mu_start = 37.64, mu_finish = 43.38, n_days = days_between_elections)
# d1 <- list(mu_start = 37.64, mu_finish = 43.38, n_days = 10)

# returns some warnings first time it compiles; see
# https://github.com/USGS-R/streamMetabolizer/issues/216
# for a similar issue.  No problem?
# http://mc-stan.org/misc/warnings.html suggests compiler
# warnings can be just ignored.
system.time({
  stan_mod1 <- stan(file = 'polls-1.stan', data = d1,
  control = list(max_treedepth = 20))
  }) # 580 seconds, with the full n_days

ex <- as.data.frame(rstan::extract(stan_mod1, "mu"))
names(ex) <- 1:d1$n_days

ex %>%
  gather(day, value) %>%
  mutate(day = as.numeric(day)) %>%
  group_by(day) %>%
  summarise(median = median(value),
            upper = quantile(value, 0.975),
            lower = quantile(value, 0.025)) %>%
  ggplot(aes(x = day)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(y = median))
  
#--------------------AC Nielson-------------------
ac <- AustralianElectionPolling %>%
  filter(org == "Nielsen") %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-09")),  # ie number of days since starting election
         p = ALP / 100,
         se_alp = sqrt(p * (1- p) / sampleSize) * 100)

d2 <- list(
  mu_start = 37.64,
  mu_finish = 43.38,
  n_days = days_between_elections,
  y_values = ac$ALP,
  y_days = ac$MidDateNum,
  y_n = nrow(ac),
  y_se = ac$se_alp
)


system.time({
  stan_mod2 <- stan(file = 'polls-2.stan', data = d2,
                   control = list(max_treedepth = 20))
}) # 80 seconds


ex <- as.data.frame(rstan::extract(stan_mod2, "mu"))
names(ex) <- 1:d2$n_days

ex %>%
  gather(day, value) %>%
  mutate(day = as.numeric(day)) %>%
  group_by(day) %>%
  summarise(median = median(value),
            upper = quantile(value, 0.975),
            lower = quantile(value, 0.025)) %>%
  ggplot(aes(x = day)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(y = median)) +
  geom_point(data = ac, aes(x = MidDateNum, y = ALP))

#-------------------all 5 polls--------------------
all_polls <- AustralianElectionPolling %>%
  mutate(MidDate = startDate + (endDate - startDate) / 2,
         MidDateNum = as.integer(MidDate - as.Date("2004-10-09")),  # ie number of days since starting election
         p = ALP / 100,
         se_alp = sqrt(p * (1- p) / sampleSize) * 100,
         org = fct_reorder(org, ALP))


poll_orgs <- as.character(unique(all_polls$org))

p1 <- filter(all_polls, org == poll_orgs[[1]])
p2 <- filter(all_polls, org == poll_orgs[[2]])
p3 <- filter(all_polls, org == poll_orgs[[3]])
p4 <- filter(all_polls, org == poll_orgs[[4]])
p5 <- filter(all_polls, org == poll_orgs[[5]])


d3 <- list(
  mu_start = 37.64,
  mu_finish = 43.38,
  n_days = days_between_elections,
  y1_values = p1$ALP,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_alp,
  y2_values = p2$ALP,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_alp,
  y3_values = p3$ALP,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_alp,
  y4_values = p4$ALP,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_alp,
  y5_values = p5$ALP,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se_alp
)


system.time({
  stan_mod3 <- stan(file = 'polls-3.stan', data = d3,
                    control = list(max_treedepth = 15,
                                   adapt_delta = 0.8),
                    iter = 4000)
})

      pairs(stan_mod3, pars = c("d", "sigma"))

ex <- as.data.frame(rstan::extract(stan_mod3, "mu"))
names(ex) <- 1:d3$n_days

ex %>%
  gather(day, value) %>%
  mutate(day = as.numeric(day)) %>%
  group_by(day) %>%
  summarise(median = median(value),
            upper = quantile(value, 0.975),
            lower = quantile(value, 0.025)) %>%
  ggplot(aes(x = day)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_point(data = all_polls, aes(x = MidDateNum, y = ALP, colour = org), size = 4) +
  geom_point(data = all_polls, aes(x = MidDateNum, y = ALP), shape = 1, size = 4) +
  geom_line(aes(y = median)) 
  


house_effects <- summary(stan_mod3, pars = "d")$summary %>%
  as.data.frame() %>%
  round(2) %>%
  mutate(org = poll_orgs) %>%
  dplyr::select(org, mean, se_mean, `2.5%`, `97.5%`)
house_effects
# small choices of prior make quite a big difference eg the prior 
# for variance of the innovations


summary(stan_mod3, pars = "sigma")$summary
