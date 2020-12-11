#Gross Error Contamination Model
x1 <- rnorm(n1)
x1[1:(n1*eps)] <- (x1[1:(n1*eps)]+3*sample(c(-1,1),size = (n1*eps),replace = T))/0.1

