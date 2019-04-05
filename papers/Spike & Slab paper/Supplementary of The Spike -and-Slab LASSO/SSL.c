#include <math.h>
#include <string.h>
#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>


#include <stdio.h>



SEXP SSL_gaussian(SEXP X_, SEXP y_, SEXP penalty_, SEXP lambda0s_, SEXP eps_, SEXP max_iter_, SEXP lambda1_, SEXP theta_);

SEXP standardize(SEXP X_);


// Cross product of y with jth column of X
double crossprod(double *X, double *y, int n, int j) {

  int nn = n*j;

  double val=0;

  for (int i=0;i<n;i++) val += X[nn+i]*y[i];

  return(val);
}



// Sum of squares of jth column of X
double sqsum(double *X, int n, int j) {

  int nn = n*j;

  double val=0;

  for (int i=0;i<n;i++) val += pow(X[nn+i], 2);

  return(val);

}

double sum(double *x, int n) {

  double val=0;

  for (int i=0;i<n;i++) val += x[i];

  return(val);
}

int checkConvergence(double *beta, double *beta_old, double eps, int l, int J) {

  int converged = 1;

  for (int j=0; j<J; j++) {

    if (fabs((beta[l*J+j]-beta_old[j])/beta_old[j]) > eps) {

      converged = 0;

      break;

    }

  }

  return(converged);
}


double pstar(double x, double theta, double lambda1, double lambda0){

  double value;

  if (lambda1==lambda0){return 1;} else{

  value=(1-theta)/theta*lambda0/lambda1*exp(-fabs(x)*(lambda0-lambda1));

  value+=1;

  value=1/value;

  return value;}


}




double expectation_approx(double *beta, double a, double b, int p, int l){

  int sum=0;

  int i;
  
  for (i=0;i<p;i++){
    
    if(beta[l*p+i]!=0){sum++;}
  
  } 

  return (sum+a)/(a+b+p);

}




double lambdastar(double x, double theta, double lambda1, double lambda0){

  double aux;

  if (lambda1==lambda0){return lambda1;} else{

    aux=pstar(x,theta,lambda1,lambda0);

    return aux*lambda1+(1-aux)*lambda0;}

}



double g(double x, double theta, double lambda1, double lambda0, double n){

  double value=lambdastar(x,theta,lambda1,lambda0);

  return pow((value-lambda1),2)+2*n*log(pstar(x,theta,lambda1,lambda0));
}




double penalty(double x, double theta, double lambda1, double lambda0){

  return -lambda1*fabs(x)+log(pstar(0,theta,lambda1,lambda0))-log(pstar(x,theta,lambda1,lambda0));
}



double find_delta(double x, void *params){

  double n= ((double*)params)[0];

  double theta= ((double*)params)[1];

  double lambda1= ((double*)params)[2];

  double lambda0= ((double*)params)[3];
  
  return n*x/2-penalty(x,theta,lambda1,lambda0)/x;
}





double threshold(double theta, double lambda1, double lambda0, int n, int approximate)
{

 
  if (lambda0==lambda1){return lambda1;} else{

  if( g(0,theta,lambda1,lambda0,n)>0){

    return sqrt(2*n*log(1/pstar(0,theta,lambda1,lambda0)))+lambda1;

  }
  
  else{
  
    return lambdastar(0,theta,lambda1,lambda0);

   
  }

  }
}






double SSL(double z, double beta, double lambda0, double lambda1, double theta, double v, int n, double delta) {

  double s=0;

  double lambda;


  if (z > 0) s = 1;

  else if (z < 0) s = -1;

  if (fabs(z) <= delta) return(0);

  else {
    
    lambda=lambdastar(beta,theta, lambda1,lambda0);

    return (fabs(z)-lambda)*s/n;
   
  }
}



static R_CallMethodDef callMethods[] = {

  {"SSL_gaussian", (DL_FUNC) &SSL_gaussian, 12},

  {"standardize", (DL_FUNC) &standardize, 1},


  {NULL, NULL, 0}

};

void R_init_ncvreg(DllInfo *info) {

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);

}
