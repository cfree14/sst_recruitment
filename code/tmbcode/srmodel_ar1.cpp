
// TMB code
#include <TMB.hpp>

// Objective function
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_INTEGER(Nstocks);
  DATA_INTEGER(Nobs);
  DATA_IVECTOR(StockID);
  DATA_VECTOR(SB_t);
  DATA_VECTOR(R_t);
  DATA_INTEGER(SR_type);

  // Parameters
  PARAMETER_VECTOR(ln_alpha);
  PARAMETER_VECTOR(ln_beta);
  PARAMETER_VECTOR(ln_sigmaR);
  PARAMETER_VECTOR(rho);

  // Transform parameters
  int i;
  Type nll=0;
  vector<Type> alpha(Nstocks);
  vector<Type> beta(Nstocks);
  vector<Type> sigmaR(Nstocks);
  for(int i=0; i<Nstocks; i++){
    alpha(i) = exp(ln_alpha(i));
    beta(i) = exp(ln_beta(i));
    sigmaR(i) = exp(ln_sigmaR(i));
  }
  
  // Likelihood calculation
  vector<Type> eps_t(Nobs); // residuals
  vector<Type> R_t_exp(Nobs); // predicted recruitment
  // Calculate first prediction and residual
  R_t_exp(0) = alpha(StockID(0)) * SB_t(0) * exp(-beta(StockID(0))*SB_t(0)); // Ricker
  eps_t(0) = R_t(0) - R_t_exp(0);
  nll -= dnorm(eps_t(0), Type(0.0), sigmaR(StockID(0)), true); 
  // Ricker
  if(SR_type==0){
    for(int i=1; i<Nobs; i++){
      R_t_exp(i) = alpha(StockID(i)) * SB_t(i) * exp(-beta(StockID(i))*SB_t(i));
      eps_t(i) = R_t(i) - R_t_exp(i);
      if(StockID(i) != StockID(i-1)){
        nll -= dnorm(eps_t(i), Type(0.0), sigmaR(StockID(i)), true);
      }else{
        nll -= dnorm(eps_t(i), rho(StockID(i))*eps_t(i-1), sigmaR(StockID(i)), true);
      }    
    }
  }
  // Beverton-Holt
  if(SR_type==1){
    for(int i=0; i<Nobs; i++){
      R_t_exp(i) = alpha(StockID(i)) * SB_t(i) / (beta(StockID(i))+SB_t(i) ); 
      nll -= dnorm( log(R_t(i)), log(R_t_exp(i))-pow(sigmaR(StockID(i)),2)/2, sigmaR(StockID(i)), true);
    }
  }
  
  // Report transformed parameters  
  ADREPORT( alpha );
  ADREPORT( beta );
  ADREPORT( sigmaR );

  return nll;
  
}
