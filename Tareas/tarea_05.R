S <- matrix(c(10,4,-5,4,12,-3,-5,-3,4),nrow = 3, ncol = 3)
S

p <- 3
n <- 20

trS <- sum(diag(S))
sigma_squared <- trS / p

det_S <- det(S)
det_S

lambda <- n*p*(log(sigma_squared) - log(det_S))
lambda

df <- ((p + 2)*(p - 1))/2
df

alpha = 0.01

chisq <- qchisq(1-alpha, df, lower.tail = F)
chisq
