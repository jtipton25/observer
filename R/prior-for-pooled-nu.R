layout(matrix(1:3, 1, 3))
mu=rbeta(1000, 5,5)
hist(mu)
eta=rgamma(1000,10,0.1)
hist(eta)
alpha=mu*eta
beta=(1-mu)*eta
hist(2/rbeta(1000, alpha, beta))
