library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat <- read.csv('track_dat.csv')

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
