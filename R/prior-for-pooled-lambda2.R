layout(matrix(1:4, 2, 2))
mu= rlnorm(1000,0,1)
hist(mu)
s=runif(1000,0,1)
hist(s)
alpha=mu^2/s^2
beta=mu/s^2
hist(rgamma(1000, alpha, beta), main="implied prior", breaks=100)
hist(rgamma(10000, 1, 1/1.78), main="Park and Cassella Prior", breaks=100)
