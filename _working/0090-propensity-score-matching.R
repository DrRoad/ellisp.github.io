library(tidyverse)
library(MASS) # for mvrnorm
library(clusterGeneration)
library(boot) # for inv.logit
#' generate data suitable for modelling with propensity score matching
#' @param n number of observations to make
#' @param k number of "x" variables that influence both the probability of getting the intervention,
#' and the response variable
#' @param p overall probability of getting the intervention
#' @param x_coefs_y the coefficients for each of the x variables impacting on y
#' @param x_coefs_propensity the coefficients for each of the x variables impacting on the probability 
#' of getting the intervention (prior to scaling proportions)
#' @param intervention_coef the effect of the intervention on y
#' @param z an unobserved latent variable length n that impacts on the true probability of getting the intervention
#' 
generate_data <- function(n = 50, k = 5, p = 0.1, 
                          x_coefs_y = rnorm(k), 
                          x_coefs_propensity = rnorm(k),
                          intervention_coef = 1,
                          z = rep(0, n),
                          seed = NULL){
   if(!is.null(seed)){
      set.seed(seed)
   }
   sigma <- cov2cor(clusterGeneration::genPositiveDefMat(k)$Sigma)
   x <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = sigma)
   true_prob <- boot::inv.logit(x %*% x_coefs_propensity)
   # scale so only p proportion will get the intervention
   true_prob <- true_prob * (p / (sum(true_prob) / n))
   intervention <- rbinom(n = n, size = 1, prob = true_prob)
   y <- x %*% x_coefs_y + intervention * intervention_coef
   the_data <- as.data.frame(x)
   the_data$intervention <- intervention
   the_data$y <- y
   return(the_data)
}

generate_data()
