#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
using namespace Rcpp;
using namespace std;

#define GETV(x, i) x[i % x.length()]

// log-pdf unit-gamma

inline double logpdf_ugamma(double x, double lnx, double mu, double phi)
{
  double t1 = pow(mu, 1/phi);
  double t2 = 1 - t1;
  double dt = t1/t2;
  double t3 = log(dt);
  double t4 = lgamma(phi);
  double t5 = pow(x, phi);
  double t6 = dt - 1;
  double t7 = lnx;
  double t8 = phi - 1;
  double t9 = log(-t7);
  return(phi * t3 - t4 + t6 * t7 + t8 * t9);
}

// [[Rcpp::export]]
NumericVector dugamma_cpp(const NumericVector x,
                          const NumericVector mu,
                          const NumericVector phi,
                          const bool logprob = false)
{
  const int n = x.length(); NumericVector out(n);
  const int nmu = mu.length(); const int nph = phi.length();
  
  for(int i = 0; i < n; i++)
    out[i] = logpdf_ugamma(x[i], log(x[i]), mu[i % nmu], phi[i % nph]);

  if(logprob) return(out); else return(Rcpp::exp(out));
}


// cdf unit-gamma
inline double cdf_ugamma(double x, double mu, double phi)
{
  double t1 = pow(mu, 1/phi);
  double t2 = 1 - t1;
  double dt = t1/t2;
  double t3 = -log(x);
  double p = R::pgamma(t3, phi, 1/dt, FALSE, FALSE);
  return(p);
}

// [[Rcpp::export]]
NumericVector pugamma_cpp(const NumericVector x,
                          const NumericVector mu,
                          const NumericVector phi,
                          const bool lowertail = true,
                          const bool logprob = false)
{
  const int n = x.length(); 
  NumericVector out(n);
  const int nmu = mu.length(); 
  const int nph = phi.length(); 

  for(int i = 0; i < n; i++)
    out[i] = cdf_ugamma(x[i], mu[i % nmu], phi[i % nph]);

  if (!lowertail) out = 0.1e1 - out;
  if (logprob) out = Rcpp::log(out);
  return(out);
}


// inv-cdf unit-gamma
inline double invcdf_ugamma(double x, double mu, double phi)
{
  double t1 = pow(mu, 1/phi);
  double t2 = 1 - t1;
  double dt = t1/t2;
  double p = exp(-R::qgamma(x, phi, 1/dt,  FALSE, FALSE));
  return(p);
}

// [[Rcpp::export]]
NumericVector qugamma_cpp(const NumericVector x,
                          const NumericVector mu,
                          const NumericVector phi,
                          const bool lowertail = true,
                          const bool logprob = false)
{
  const int n = x.length(); 
  NumericVector out(n);
  const int nmu = mu.length(); 
  const int nph = phi.length();

  if(lowertail)
  {
    for(int i = 0; i < n; i++)
      out[i] = invcdf_ugamma(x[i], mu[i % nmu], phi[i % nph]);
  }
  else
  {
    for(int i = 0; i < n; i++)
      out[i] = invcdf_ugamma(0.1e1 - x[i], mu[i % nmu], phi[i % nph]);
  }
  if(logprob) return(Rcpp::log(out)); else return(out);
}

// random number unit-gamma
inline double rng_ugamma(double mu, double phi) {
  double t1 = pow(mu, 1/phi);
  double t2 = 1 - t1;
  double t3 = t1/t2;
  return exp(-R::rgamma(phi, 1/t3));
}

// [[Rcpp::export]]
NumericVector rugamma_cpp(
    const int& n,
    const NumericVector& mu,
    const NumericVector& phi
  ) {
  
  NumericVector out(n);
  const int nmu = mu.length(); 
  const int nph = phi.length();
  
  for (int i = 0; i < n; i++)
    out[i] = rng_ugamma(mu[i % nmu], phi[i % nph]);
  return out;
}

