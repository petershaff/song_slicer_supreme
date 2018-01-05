library(rstan)
library(tidyverse)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#IMPORT DATA
beats <- read.csv('./data/beats.csv')
bleep <- read.csv('./data/bleep.csv')
bangz <- read.csv('./data/bangz.csv')
rando <- read.csv('./data/rando.csv')

#BUILD TRAINING and TESTING DATA SETS
train.samp.size <- 10

train <- rbind( beats[ sample(nrow(beats), train.samp.size), ], bleep[ sample(nrow(beats), train.samp.size), ], bangz[ sample(nrow(beats), train.samp.size), ], rando[ sample(nrow(beats), 50), ] )
test <- rbind( beats[ sample(nrow(beats), (nrow(beats) - train.samp.size) ), ], bleep[ sample(nrow(bleep), (nrow(bleep) - train.samp.size) ), ], bangz[ sample(nrow(bangz), (nrow(beats) - train.samp.size) ), ], rando[ sample(nrow(rando), (nrow(rando) - 50)), ])


#All options for covariate names are: 'danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'pitch', 'timbre', 'plylst_lbl'
covar.names <- c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'pitch', 'timbre')



X <- dat[,covar.names]
Y <- dat$'plylst_lbl'
N <- dim(dat)[1]
K <- length(covar.names)

#Fit vanilla model in Stan
mod.file <- './stan_scripts/vanilla.stan'

iter  <- 4000
stan.dat <- list(N=N, K=K, X=X, Y=Y)   

print('init vanilla_mod...') 
stan.mod <- stan(mod.file, data=stan.dat, chains=4)
print('...beginning vanilla_mod fit...') 
stan.fit <- stan(fit=stan.mod, data=stan.dat, iter=iter)        
print('...done')
