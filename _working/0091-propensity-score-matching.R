library(tidyverse)
library(magrittr)
library(forcats)
library(scales)
library(MatchIt)           # for propensity score matching
library(MASS)              # for mvrnorm
library(clusterGeneration) # for genPositiveDefMat
library(boot)              # for inv.logit
library(testthat)          # for expect_equal
library(doParallel)        # for parallel processing
library(foreach)




#==========explore omitted variable bias==================
#' generate data suitable for modelling with propensity score matching
#' @param n number of observations to make
#' @param k number of "x" variables that influence both the probability of getting the treatment,
#' and the response variable
#' @param p overall probability of getting the treatment
#' @param x_coefs_y the coefficients for each of the x variables impacting on y
#' @param x_coefs_propensity the coefficients for each of the x variables impacting on the probability 
#' of getting the treatment (prior to scaling proportions)
#' @param treatment_coef the effect of the treatment on y
#' @return a data frame
generate_data <- function(n = 50, k = 5, p = 0.1, 
                          x_coefs_y = rnorm(k), 
                          x_coefs_propensity = rnorm(k),
                          treatment_coef = 1,
                          sigma_y = 1,
                          seed = NULL){
   if(!is.null(seed)){
      set.seed(seed)
   }
   sigma <- cov2cor(clusterGeneration::genPositiveDefMat(k)$Sigma)
   x <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = sigma)
   true_prob <- boot::inv.logit(x %*% x_coefs_propensity)
   # scale so only p proportion will get the treatment
   true_prob <- true_prob * (p / (sum(true_prob) / n))
   treatment <- rbinom(n = n, size = 1, prob = true_prob)
   y <- x %*% x_coefs_y + treatment * treatment_coef + rnorm(n, 0, sigma_y)
   the_data <- as.data.frame(x)
   the_data$treatment <- treatment
   the_data$y <- as.vector(y)
   return(the_data)
}

#=====================loop=================
# sequence

# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(foreach)
   library(tidyverse)
   library(MatchIt)
   library(magrittr)
})

# Different sample sizes to try:
n_options <- c(500, 1000, 2000, 5000, 10000, 20000, 10^5)
# n_options <- c(500, 1000, 2000) # used during dev
# Different number of variables to include:
var_options <- 1:10 * 10
# Number of reps to do of each combination of sample size and variables in:
reps <- 30

# loop through all the options for sample size.  Note this loop is serial, not parallel:
results <- foreach(j = 1:length(n_options), .combine = rbind) %do% {
   
   # parallelising this next loop, but not the others.  So each rep of the given
   # sample size will be given its own processor:
   tmp1 <- foreach(i = 1:reps, .combine = rbind) %dopar% {
      the_data <- generate_data(n = n_options[j], k = 100, seed = i * 100 + j)
      
      # treatments <- dplyr::filter(the_data, treatment == 1)
      # controls <- dplyr::filter(the_data, treatment == 0)
      # n_treatment <- nrow(treatments)
      
      # Loop through all the different numbers of observed explanatory variables.
      # Note this is done in serial too, so a single processor, given its own
      # rep dataset of a particular sample size will perform all the different
      # matchings and regressions for different selections of variables.  This
      # is hoped to be a good compromise between overall CPU utilisation and
      # not swapping data and stuff in and out of processors too frequently:
      tmp2 <- foreach(v = 1:length(var_options), .combine = rbind) %do% {
         vars_in <- 1:var_options[v]
         incomplete_data <- dplyr::select(the_data, vars_in, treatment, y)
         
         #-----------------Simple regression-----------
         mod1 <- lm(y ~ ., data = incomplete_data)
         result_lm <- coef(mod1)["treatment"]
         
         #----------propensity score calculation----------
         the_form <- as.formula(
            paste("treatment ~ ", paste(paste0("V", vars_in), collapse = " + "))
         )
         
         # I considered comparing genetic matching but it is slower and not really the point
         # of today's piece, so just using the default nearest neighbour matching method
         match_model <- matchit(the_form, data = incomplete_data)
         match_data <- match.data(match_model)
         
         #---------------compare means of matched groups----------------
         result_psm <- with(match_data,
              mean(y[treatment == 1]) -
                 mean(y[treatment == 0]))
         
         #--------------regression with matched groups-----------------
         mod2 <- lm(y ~ ., data = match_data)
         result_psm_lm <- coef(mod2)["treatment"]

         #--------------------regression with full data and IPTW weights----------
         # IPTW - Inverse Probability of Treatment Weights
         # keep all the data but use propensity to generate weights.
         # has the advantage of keeping all the data rather than just a "matched" control sample.
         wgts <- incomplete_data %>%
            mutate(props = predict(match_model$model, newdata = incomplete_data, type = "response"),
                   wgts  = 1/ props * treatment + 1 / (1 - props) * ( 1 - treatment)) %$%
            wgts
         
         mod3 <- lm(y ~ ., data = incomplete_data, weights = wgts)
         result_iptw_lm <- coef(mod3)["treatment"]
         
         #--------------return results-------------
         c(result_lm, result_psm, result_psm_lm, result_iptw_lm)
      }  # end and repeat for each value of the number of explanatory variables to include in `var_options`
      
      tmp2 <- as.data.frame(tmp2)
      tmp2$n <- n_options[j]
      tmp2$vars_in <- var_options
      tmp2$dataset <- i
      row.names(tmp2) <- NULL
      tmp2
   }     # end and repeat for each of `reps` repetitions of new datasets of same sample size
   
   tmp1
}        # end and repeat for each of sample size options in `n_options``

names(results)[1:4] <- c("Straight regression", "Propensity matching",
                         "Propensity matching and regression", "Inverse weighting and regression")

#=============================presentation====================
results %>%
   filter(n == max(n)) %>%
   gather(method, value, -vars_in, -dataset, -n) %>%
   mutate(n = paste0("n =", format(n, big.mark = ",", scientific = FALSE)),
          dataset = paste0("Data ", dataset),
          method = fct_reorder(method, value)) %>%
   ggplot(aes(x = vars_in, y = value, colour = dataset)) +
   geom_hline(yintercept = 1) +
   geom_line() +
   facet_grid(method~n) +
   scale_y_continuous(limits = c(-1, 3)) 

# i don't understand why it seems to do better with more variables missing
results %>%
#   filter(vars_in %in% c(10, 100)) %>%
   gather(method, value, -vars_in, -dataset, -n) %>%
   mutate(AbsError = abs(value - 1)) %>%
   mutate(method = fct_reorder(method, -AbsError)) %>% 
   ggplot(aes(x = n, y = AbsError, colour = method)) +
   geom_smooth(se = FALSE, method = "loess", span = 1.5) +
   geom_point() +
   scale_x_log10(label = comma, breaks = unique(results$n)) +
   scale_y_log10(label = comma) +
   facet_wrap(~vars_in) +
   theme(legend.position = "right",
         panel.grid.minor = element_blank()) +
   ggtitle("Mean squared error of estimated treatment effect",
           "Comparing propensity matching and weighting to a straight regression") +
   labs(x = "Sample size", colour = "") 


