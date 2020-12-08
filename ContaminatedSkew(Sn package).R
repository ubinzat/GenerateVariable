library(sn)

#The SN family only supports skew between -0.99527 and 0.99527.
#Outside of this range, the ST family is needed, 
#which requires a fourth variable: kurtosis:
#cp2dp(c(0, 3, -0.99), "SN") # rsn fonksiyonu için verilen degerlerin dönüsümünü yapiyor.

####rst
cont_skew <- function(n, mu = 0,sd1 = 1, sd2 = 9, prob = eps, sk=-1.2,kur=2.37) {
  x1 <- vector(mode = "numeric", length = n)
  s <- sample(c(sd1, sd2), n, replace = T, prob = c(1 - prob, prob))
  rnorm(n, mean = mu, sd = s)
  for(i in 1:length(s)){
    x1[i] <- rst(dp = cp2dp(c(mu, s[i], sk, kur), "ST"))
  }
  return(x1)
}


######rsn
cont_skew <- function(n, mu = 0,sd1 = 1, sd2 = 9, prob = eps, sk=-0.9) {
  x1 <- vector(mode = "numeric", length = n)
  s <- sample(c(sd1, sd2), n, replace = T, prob = c(1 - prob, prob))
  rnorm(n, mean = mu, sd = s)
  for(i in 1:length(s)){
    x1[i] <- rsn(dp = cp2dp(c(mu, s[i], sk), "SN"))
  }
  return(x1)
}

#cont_skew(n = 10,mu = 0,sd1 = 1,sd2 = 100,prob = 0.9)



