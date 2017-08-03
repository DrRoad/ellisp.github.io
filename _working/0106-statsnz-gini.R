library(tidyverse)
library(acid)
library(microbenchmark)
library(forcats)



# John and Draper's modulus transformation
modulus_trans <- function(lambda){
  trans_new("modulus",
            transform = function(y){
              if(lambda != 0){
                yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
              } else {
                yt = sign(y) * (log(abs(y) + 1))
              }
              return(yt)
            },
            inverse = function(yt){
              if(lambda != 0){
                y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
              } else {
                y <- (exp(abs(yt)) - 1) * sign(yt)
                
              }
              return(y)
            }
  )
}


.mod_transform <- function(y, lambda){
  if(lambda != 0){
    yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
  } else {
    yt = sign(y) * (log(abs(y) + 1))
  }
  return(yt)
}


.mod_inverse <- function(yt, lambda){
  if(lambda != 0){
    y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
  } else {
    y <- (exp(abs(yt)) - 1) * sign(yt)
    
  }
  return(y)
}

prettify <- function(breaks){
  # round numbers, more aggressively the larger they are
  digits <- -floor(log10(abs(breaks))) + 1
  digits[breaks == 0] <- 0
  return(round(breaks, digits = digits))
}

mod_breaks <- function(lambda, n = 8, prettify = TRUE){
  function(x){
    breaks <- .mod_transform(x, lambda) %>%
      pretty(n = n) %>%
      .mod_inverse(lambda)
    if(prettify){
      breaks <- prettify(breaks)
    }
    return(breaks)
  }
}


StatsGini <- function(x, w = rep(1, length(x))){
  # x and w are vectors
  # w can be left blank when calling the fn (i.e. no weighting)
  # Examples:
  # x <- c(3, 1, 7, 2, 5)
  # w <- c(1, 2, 3, 4, 5)
  # StatsGini(x, w) should yield 0.2983050847
  # StatsGini(c(0.25, 0.75), c(1, 1)) should yield 0.25
  n <- length(x)
  wxsum <- sum(w * x)
  wsum <- sum(w)
  sxw <- order(x, w) # Ascending order sort
  sx <- w[sxw] * x[sxw]
  sw <- w[sxw]
  pxi <- vector(mode = "numeric", length = n)
  pci <- vector(mode = "numeric", length = n)
  pxi <- cumsum(sx) / wxsum
  pci <- cumsum(sw) / wsum
  G <- 0.0
  for (i in 2:n){
    G <- G - (pci[i] * pxi[i - 1] - pxi[i] * pci[i - 1] )
  }
  return(G)
}


options(digits = 10)
x <- c(3, 1, 7, 2, 5)
w <- c(1, 2, 3, 4, 5)
StatsGini(x, w) # should yield 0.2983050847
StatsGini(c(0.25, 0.75), c(1, 1)) # should yield 0.25

weighted.gini(x, w)
weighted.gini(c(0.25, 0.75), c(1, 1))

#' Generate a realistic mixed distribution of incomes
#'
#' alpha ~ t(d_alpha) scaled to mean, sd of (log(mu), sigma_alpha)
#' beta ~ t(d_beta) scaled to mean, sd of (alpha, sigma_beta)
#' x = exp(beta)
#' y = x * I(-1, 0, 1)
#'
#' @param n sample size to generate
#' @param mu median income
#' @param sigma_alpha standard deviation of underlying latent means
#' @param sigma_beta standard deviation of scaled t distribution underlying the population's log-t distribution
#' @param d_alpha degrees of freedom in th
#' @param d_beta degrees of freedom in the t distribution underlying the log-t
#' @param pr vector of 2 elements, the probability of negative income and of zero income respectively
gen_incomes <- function(n, mu = 30000, 
                        sigma_alpha = 1, sigma_beta = 1, 
                        d_alpha = 15, d_beta = 15,
                        pr = c(0.01, 0.2)){
  alpha <- rt(n, d_alpha) * sigma_alpha + log(mu)
  beta <- exp(rt(n, d_beta) * sigma_beta + alpha)
  y <- beta * sample(c(-1, 0, 1), n, replace = TRUE, prob = c(pr, 1 - sum(pr)))
  return(y)
}

set.seed(123)
N <- 100000
population <- data.frame(income = gen_incomes(N), sigma_alpha = 1, d_alpha = 10, d_beta = 150,
                         sigma_beta = 2,
                         prob = rbeta(n, 2, 2))

# best value of lambda for transforming? Turns out to be very low, so almost like doing a log transform
forecast::BoxCox.lambda(sample(abs(population$income), 1000))

# breaks look dreadful, use the approach at http://ellisp.github.io/blog/2015/09/07/transforming-breaks-in-a-scale
# instead.
svg("../img/0106-density.svg", 7, 4)
population %>%
  slice(1:1000) %>%
  ggplot(aes( x = income)) + 
  scale_x_continuous("Annual income (modulus transform with power of 0.02)",
                     label = dollar, 
                     trans = modulus_trans(0.02),
                     breaks = mod_breaks(lambda = 0.02)) + 
  geom_density(fill = "grey", alpha = 0.2)  +
  geom_rug() +
  ggtitle("Density of simulated annual incomes",
          "First 1,000 values")
dev.off()

small_sample <- population[1:100, ]
weighted.gini(small_sample$income, w = 1 / small_sample$prob)$Gini
StatsGini(small_sample$income, w = 1 / small_sample$prob)

y <- population$income
w <- 1/ population$prob
mb <- microbenchmark(
  weighted.gini(y, w)$Gini,
  StatsGini(y, w),
  times = 1000)
mb

svg("../img/0106-benchmark.svg", 8, 4)
  autoplot(mb, log = FALSE) +
    ggtitle("Time taken to estimated Gini coefficient from 100,000 observations",
            "Microbenchmark results") 
dev.off()


# number of repeats at each sample size:
reps <- 100
# sample sizes, chosen to resemble realistic survey sizes:
ns <- c(30, 100, 500, 1000, 3000, 10000)

res <- matrix(0, nrow = reps, ncol = length(ns))

# obviously this next bit could be made much more efficient by parallel processing,
# but it's only a few minutes of running 
set.seed(666)
for(i in 1:length(ns)){
  n <- ns[i]
  for(j in 1:reps){
    the_sample <- sample_n(population, size = n, replace = FALSE, weight = population$prob)    
    
    res[j, i] <- weighted.gini(the_sample$income, w = 1 / the_sample$prob)$Gini
   }
}

res_df <- as.data.frame(res)
names(res_df) <- format(ns, big.mark = ",")

svg("../img/0106-sample-distributions.svg", 8, 6)
res_df %>%
  gather(sample_size, value) %>%
  mutate(sample_size = fct_reorder(sample_size, -value)) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~sample_size) +
  geom_density(alpha = 0.3, fill = "grey") +
  # a few values of > 1 make no sense and warp the graph:
  coord_cartesian(xlim = c(0.6, 1)) +
  labs(x = "Estimated weighted Gini") +
  ggtitle("Sampling distribution of weighted Gini point estimate from a complex survey",
          paste0("Various sample sizes; true value is ", round(weighted.gini(the_sample$income)$Gini, 3)))

dev.off()

# note might be a bug in dplyr, format doesn't work in the pipeline?
