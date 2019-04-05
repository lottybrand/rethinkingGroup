#chapter 4 code

devtools::install_github("rmcelreath/rethinking",ref="Experimental")

library(rethinking) 
data(Howell1)
d <- Howell1
precis(d)

d2 <- d[ d$age >= 18 , ]


# prior predictive distribution
# what do priors say about the height, before we see the data? Simulate and see! 
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

#calculate the grid approximation ourselves as we did in previous chapter. More complicated than the globe tossing example now
# the rest of the course uses an approximation of it which is quadratic approximation 

mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

#using quap:

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- quap( flist , data=d2 )

precis(m4.1)

plot( d2$height ~ d2$weight )

# now add a predictor! weight: 

# define the average weight, x-bar
xbar <- mean(d2$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

precis( m4.3 )
