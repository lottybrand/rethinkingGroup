# page 52 onwards of second edition

#Rcode 3.2:

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

plot( samples )

library(rethinking)
dens( samples )

# add up posterior probability where p < 0.5 3.6
sum( posterior[ p_grid < 0.5 ] )

sum( samples < 0.5 ) / 1e4

sum( samples > 0.5 & samples < 0.75 ) / 1e4

quantile( samples , 0.8 )

quantile( samples , c( 0.1 , 0.9 ) )

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

PI( samples , prob=0.5 )

HPDI( samples , prob=0.5 )

p_grid[ which.max(posterior) ]

### skipping to 3.3: Sampling to simulate prediction

dbinom( 0:2 , size=2 , prob=0.7 )

rbinom( 1 , size=2 , prob=0.7 )

rbinom( 10 , size=2 , prob=0.7 )

dummy_w <- rbinom( 1e5 , size=2 , prob=0.7 )
table(dummy_w)/1e5

dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 )
simplehist( dummy_w , xlab="dummy water count" )
