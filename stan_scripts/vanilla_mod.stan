//DATA
data {
int<lower=0> N;
int<lower=0> K;

matrix[N,K] X; //covariates
int<lower=0,upper=1> Y[N]; //class labels
 
}

//PARMS
parameters {
vector[K] beta; //model coefficients
real beta0; //bias

}

//TRNSPARMS
transformed parameters{

}

//MODEL
model {
vector[N] z; // class logit-probabiliies

z = X*beta + beta0;
Y ~ bernoulli_logit(z);

}

//GEN'D QUANTS
generated quantities {
}
