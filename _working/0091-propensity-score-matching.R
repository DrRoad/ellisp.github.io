

#============roll your own nearest neighbour matching with replacement===========
#' Helper function for matching a number to a vector
#' @param x1 single number
#' @param x2 vector
#' @value index indicating which element of x2 is closest to x1
min_diff = function(x1, x2){ 
   if(length(x1) != 1){stop("x1 should be a single number")}
   which.min(abs(x1 - x2)) 
} 
expect_equal(min_diff(4.3, 1:10), 4)

model <- glm(treat ~ age + educ + black + hispan + nodegree + married +  re74 + re75, 
             data = lalonde, family = "binomial")
# Compare this to the model used by matchit earlier.  Identical:
cbind(coef(match_model$model), coef(model))

lalonde$prop_scores <- predict(model, type = "link")

lalonde %>%
   mutate(treat = as.factor(treat)) %>%
   ggplot(aes(x = prop_scores, fill = treat))  + 
   geom_density(alpha = 0.5) +
   ggtitle("Propensity for getting the treatment",
           "split by whether the subject actually did get the treatment or not") +
   labs(x = "Propensity to get the treatment (logit scale, so 0 means 50% chance)",
        caption = "Lalonde's 1986 data on a job training program")


untreated_group <- filter(lalonde, treat == 0)
treatment_group <- filter(lalonde, treat == 1)

matches <- sapply(treatment_group$prop_scores, min_diff, untreated_group$prop_scores)
expect_equal(length(matches), nrow(treatment_group))

control_group <- untreated_group[matches, ]

rbind(control_group[1, ], treatment_group[1, ])
rbind(control_group[2, ], treatment_group[2, ])
rbind(control_group[3, ], treatment_group[3, ])

# Compare the two groups:
c(mean(control_group$re78), mean(treatment_group$re78))
# Or by removing the duplicates from the control group where more than one
# treatment matched (because this seems to be how MatchIt does it):
c(mean(distinct(control_group)$re78), mean(treatment_group$re78))

# Choose one of the large variety of propensity score matching methods to model propensity
# Note - there is some randomness here, and using set.seed() doesn't make it reproducible:
matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
        data = lalonde, method = "nearest", replace = TRUE) %>%
   match.data() %>%
   group_by(treat) %>%
   summarise(Income1978 = mean(re78),
             n = n())


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

the_data <- generate_data(n = 1000, k = 100, seed = 124)

# full, correct model.  
# True value of treatment's coefficient is 1
mod <- lm(y ~ ., data = the_data)
confint(mod)["treatment", ]


#=====================loop=================
clusterEvalQ(cluster, {
   library(foreach)
})
ns <- c(500, 1000, 2000, 5000, 10000, 20000, 10^5)
#ns <- c(10000)
#ns <- c(500, 1000, 2000)

results <- foreach(j = 1:length(ns), .combine = rbind) %do% {
   tmp1 <- foreach(i=1:7, .combine = rbind) %dopar% {
      the_data <- generate_data(n = ns[j], k = 100, seed = i * 100 + j)
      
      treatments <- dplyr::filter(the_data, treatment == 1)
      controls <- dplyr::filter(the_data, treatment == 0)
      n_treatment <- nrow(treatments)
      
      tmp2 <- foreach(vars_in = 1:100, .combine = rbind) %do% {
         
         
         mod <- lm(y ~ ., data = dplyr::select(the_data, 1:vars_in, treatment, y))
         result_lm <- coef(mod)["treatment"]
         
         # propensity score matching
         prop_model <- glm(treatment ~ ., family = "quasibinomial", 
                           data = dplyr::select(the_data, 1:vars_in, treatment))
         
         propensity_control <- predict(prop_model,   newdata = controls,   type = "response")
         propensity_treatment <- predict(prop_model,   newdata = treatments,   type = "response")
         
         # probabilistic sampling
         samp_control <- controls[sample(names(propensity_control), size = n_treatment, 
                                         replace = FALSE, prob = propensity_control), ]
         result_pm1 <- mean(treatments$y) - mean(samp_control$y)
         
         # matching by nearest neighbour
         propensity_control_logit <- predict(prop_model,   newdata = controls,   type = "link")
         propensity_treatment_logit <- predict(prop_model,   newdata = treatments,   type = "link")
         
         samp_control <- controls[sapply(propensity_treatment_logit, min_diff, propensity_control_logit), ]
         result_pm2 <- mean(treatments$y) - mean(samp_control$y)
         
         # IPTW - Inverse Probability of Treatment Weights
         # keep all the data but use propensity to generate weights.
         # has the advantage of keeping all the data rather than just a "matched" control sample.
         result_pm3 <- weighted.mean(treatments$y, 1 / propensity_treatment) - 
            weighted.mean(controls$y, 1 / ( 1 - propensity_control))
         
         c(result_lm, result_pm1, result_pm2, result_pm3)
      }
      tmp2 <- as.data.frame(tmp2)
      tmp2$n <- ns[j]
      tmp2$vars_in <- 1:100
      tmp2$dataset <- i
      row.names(tmp2) <- NULL
      tmp2
   }
   tmp1
   
}

#=============================presentation====================

names(results)[1:4] <- c("Straight regression", "Probabilistic matching",
                         "Nearest matching", "Inverse weighting")

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
   filter(vars_in %in% c(10, 100)) %>%
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


