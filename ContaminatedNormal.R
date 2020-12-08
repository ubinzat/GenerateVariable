#contaminated Normal

cont_norm <- function(n, mu = 0,sd1 = 1, sd2 = 9, prob = eps) {
  
  s <- sample(c(sd1, sd2), n, replace = T, prob = c(1 - prob, prob))
  rnorm(n, mean = mu, sd = s)
}
