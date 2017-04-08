library(tidyverse)
library(forcats)
library(scales)
library(MASS)              # for mvrnorm
library(clusterGeneration) # for genPositiveDefMat
library(boot)              # for inv.logit
library(testthat)          # for expect_equal
library(MatchIt)
library(boot)              # for bootstrapping
library(doParallel)        # for parallel processing
library(foreach)

#==============example from the MatchIt vignette=================
data(lalonde)

# naive comparison - people who got the job training program
# have lower incomes in 1978 - because the training was given
# to people with income problems.  So comparison is unfair:
lalonde %>%
   group_by(treat) %>%
   summarise(Income1978 = mean(re78),
             n = n())

# Choose one of the large variety of propensity score matching methods to model propensity
match_model <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                       data = lalonde, method = "nearest")
match_data <- match.data(match_model)
   
# Simple comparison is now much fairer
match_data %>%
   group_by(treat) %>%
   summarise(Income1978 = mean(re78),
             n = n())

# regression model estimate with matched data
coef(MASS::rlm(re78 ~ age + treat + educ + black + hispan + nodegree + married +  re74 + re75, 
        data = match_data))["treat"]

# regression model estimate with original data
coef(MASS::rlm(re78 ~ age + treat + educ + black + hispan + nodegree + married +  re74 + re75, 
        data = lalonde))["treat"]



#=================bootstrap for confidence intervals=================
# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(MatchIt)
   data(lalonde)
})

#' Function to estimate treatment effect three different methods
#' @return a vector of three estimates of treatment effect: simple 
#' comparison of matched data; regression with matched data; 
#' regression with the original data.
my_function <- function(x, i){
   resampled_data <- x[i, ]
   match_data <- match.data(
      matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
              data = resampled_data, method = "nearest")
   )
   
   est1 <- with(match_data,
                mean(re78[treat == 1]) -
                   mean(re78[treat == 0]))
   
   
   # regression model estimate with matched data
   est2 <- coef(MASS::rlm(re78 ~ treat + age + educ + black + hispan + nodegree + married +  re74 + re75, 
           data = match_data))["treat"]
   
   # regression model estimate with original data
   est3 <- coef(MASS::rlm(re78 ~ treat + age + educ + black + hispan + nodegree + married +  re74 + re75, 
                   data = resampled_data))["treat"]
   return(c(est1, est2, est3))
}

my_function(lalonde, 1:nrow(lalonde))

booted <- boot(lalonde, statistic = my_function, R = 5000, 
     parallel = "snow", cl = cluster)

svg("../img/0090-job-treatment.svg", 8, 5)
booted_df <- as.data.frame(booted$t)
names(booted_df) <- c("Simple with matched data", "Regression with matched data", "Regression with original data")
booted_df %>%
   gather("Method", "Estimate") %>%
   mutate(Method = fct_reorder(Method, Estimate)) %>%
   ggplot(aes(x = Estimate, fill = Method)) +
   geom_density(alpha = 0.4, colour = "grey50") +
   scale_x_continuous(label = dollar) +
   ggtitle("Uncertainty of estimates of treatment effects from three different methods",
           "Estimated impact on income in 1978 of a job training program") +
   labs(x = "Estimated treatment effect",
        caption = "Lalonde's 1986 data on a job training program",
        fill = "")
dev.off()

# biases:
booted

# confidence intervals:
boot.ci(booted, index = 1, type = "perc")
boot.ci(booted, index = 2, type = "perc")
boot.ci(booted, index = 3, type = "perc")

