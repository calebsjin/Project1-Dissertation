/* 

Compilation:

sudo rm -r SSL2

cp -r /Users/rockova/Desktop/SSL2  /Library/Frameworks/R.framework/Versions/3.1/Resources/library/SSL2

R CMD build /Library/Frameworks/R.framework/Versions/3.1/Resources/library/SSL2

sudo  R CMD install /Library/Frameworks/R.framework/Versions/3.1/Resources/library/SSL_0.0-1.tar.gz

*/




#include <math.h>
#include <string.h>
#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>

double crossprod(double *X, double *y, int n, int j);

double sum(double *x, int n);

int checkConvergence(double *beta, double *beta_old, double eps, int l, int J);

double SSL(double z, double beta, double lambda0, double lambda1, double theta, double v, int n, double delta);

double pstar(double x, double theta, double lambda1, double lambda0);

double lambdastar(double x, double theta, double lambda1, double lambda0);

double expectation_approx(double *beta, double a, double b, int p, int l);

double threshold(double theta, double lambda1, double lambda0, int n, int approximate);

// Memory handling, output formatting (Gaussian)

SEXP cleanupG(double *a, double *r, int *e1, int *e2, double *z, double *thetas, SEXP beta, SEXP loss, SEXP iter, SEXP thetas_export) {

  Free(a);

  Free(r);

  Free(e1);

  Free(e2);

  Free(z);

  SEXP res;

  PROTECT(res = allocVector(VECSXP, 4));

  SET_VECTOR_ELT(res, 0, beta);

  SET_VECTOR_ELT(res, 1, loss);

  SET_VECTOR_ELT(res, 2, iter);

  SET_VECTOR_ELT(res, 3, thetas_export);

  UNPROTECT(5);

  return(res);
}

// Gaussian loss
double gLoss(double *r, int n) {
 
  double l = 0;
 
  for (int i=0;i<n;i++) l = l + pow(r[i],2);
 
  return(l);
}

// Coordinate descent for gaussian models

SEXP SSL_gaussian(SEXP X_, SEXP y_, SEXP penalty_, SEXP lambda0s_, SEXP eps_, SEXP max_iter_, SEXP lambda1_, SEXP theta_, SEXP counter_, SEXP approximate_, SEXP a_,
		    SEXP b_) {

  // Declarations
  
  double *X = REAL(X_);

  double *y = REAL(y_);

  int n = length(y_);

  int p = length(X_)/n;

  int L = length(lambda0s_);

  SEXP res, beta, loss, iter, thetas_export;

  PROTECT(beta = allocVector(REALSXP, L*p));

  PROTECT(thetas_export = allocVector(REALSXP, L));

  double *b = REAL(beta);

  double *thetas = REAL(thetas_export);

  for (int j=0; j<(L*p); j++) b[j] = 0;

  PROTECT(loss = allocVector(REALSXP, L));

  PROTECT(iter = allocVector(INTSXP, L));

  double theta =REAL(theta_)[0];

  int count_max =INTEGER(counter_)[0];

  double delta=0;

  for (int i=0; i<L; i++){ 
    
    INTEGER(iter)[i] = 0;

  }

  double *a = Calloc(p, double); // Beta from previous iteration

  for (int j=0; j<p; j++) a[j]=0;


  const char *penalty = CHAR(STRING_ELT(penalty_, 0));
  
  int approximate=INTEGER(approximate_)[0];

  double *lambda0s = REAL(lambda0s_);

  double eps = REAL(eps_)[0];
  
  int max_iter = INTEGER(max_iter_)[0];
  
  double *r = Calloc(n, double);
  
  for (int i=0; i<n; i++) r[i] = y[i];
  
  double *z = Calloc(p, double);
  
  for (int j=0; j<p; j++) z[j] = crossprod(X, r, n, j);
  
  int *e1 = Calloc(p, int); // Index of an active set
  
  for (int j=0; j<p; j++) e1[j] = 0;
  
  int *e2 = Calloc(p, int); // Index of an elible set from the strong rule
  
  for (int j=0; j<p; j++) e2[j] = 0;
  
  double cutoff;
  
  int converged, counter=0;

  double *thresholds = Calloc(L, double); // Thresholds for the strong screenig rule

  double aa= REAL(a_)[0];;

  double bb= REAL(b_)[0];;

  double lambda1 =REAL(lambda1_)[0];

  double lambda0;
 
  // Regularization Path

  for (int l=0;l<L;l++) {

    R_CheckUserInterrupt();

    lambda0=lambda0s[l];

    if (l != 0) {

      // Initialization: Proliferation of the previous solution 

      for (int j=0;j<p;j++) a[j] = b[(l-1)*p+j];

      if (strcmp(penalty, "adaptive")==0){

	theta=expectation_approx(b, aa, bb,p,l-1);

      }

      thresholds[l]=threshold( theta, lambda1, lambda0, n, approximate);
      
      // Determine eligible set
   
	cutoff =2*thresholds[l]-thresholds[l-1];
      
      for (int j=0; j<p; j++) if (fabs(z[j]) > (cutoff )) e2[j] = 1;
    
    } else {
    

      thresholds[l]=threshold( theta, lambda1, lambda0, n, approximate);
       
      // Determine eligible set

      double lmax = 0;
      
      for (int j=0; j<p; j++) if (fabs(z[j]) > lmax) lmax = fabs(z[j]);
      	
	cutoff =2*thresholds[l]-lmax;
      
      for (int j=0; j<p; j++) if (fabs(z[j]) > (cutoff )) e2[j] = 1;
    
}

    delta= thresholds[l];

    while (INTEGER(iter)[l] < max_iter) {

      while (INTEGER(iter)[l] < max_iter) {

	while (INTEGER(iter)[l] < max_iter) {

	  // Solve over the active set

	  INTEGER(iter)[l]++;

	  for (int j=0; j<p; j++) {


	    if (e1[j]) {

	      z[j] = crossprod(X, r, n, j) + n*a[j];

	      // Update beta_j
	   
	      b[l*p+j] = SSL(z[j], a[j],lambda0,lambda1,theta,1,n,delta);

	      // Update r
	    
	     double shift = b[l*p+j] - a[j];
	      
	     if (shift !=0) for (int i=0;i<n;i++) r[i] -= shift*X[j*n+i];

	     counter++;
	    }

	  // Update theta every count_max iterations

	  if (counter==count_max  &&  strcmp(penalty, "adaptive")==0){
	      
	    theta=expectation_approx(b, aa, bb,p,l);

	    delta= threshold( theta, lambda1, lambda0, n, approximate);

	    counter=0;
	  }

	  } 

	  // Check for convergence
	  
	  converged = checkConvergence(b, a, eps, l, p);
	  
	  for (int j=0; j<p; j++) a[j] = b[l*p+j];
	  
	  if (converged) {

	    thetas[l]=theta;

	    break;}
	}

	// Scan for violations in strong set
	
	int violations = 0;
	
	counter=0;

	
	for (int j=0; j<p; j++) {

	  if (e1[j]==0 & e2[j]==1) {

	      z[j] = crossprod(X, r, n, j) + n*a[j];

	      // Update beta_j
	  
	      b[l*p+j] = SSL(z[j], a[j],lambda0,lambda1,theta,1,n,delta);

	    // If something enters the eligible set, update eligible set & residuals

	    if (b[l*p+j] !=0) {
	      
	      e1[j] = e2[j] = 1;
	      
	      for (int i=0; i<n; i++) r[i] -= b[l*p+j]*X[j*n+i];
	      
	      a[j] = b[l*p+j];
	      
	      violations++;

	      counter++;
	    }
	  }

	  if (counter==count_max  &&  strcmp(penalty, "adaptive")==0){
	      
	    theta=expectation_approx(b, aa, bb,p,l);
	    
	    delta= threshold( theta, lambda1, lambda0, n, approximate);

	    counter=0;
	  }

	}

	if (violations==0) break;
      }

      // Scan for violations in rest

      int violations = 0;

      counter=0;

      for (int j=0; j<p; j++) {

	if (e2[j]==0) {

	    z[j] = crossprod(X, r, n, j) + n*a[j];

	      // Update beta_j

	    b[l*p+j] = SSL(z[j], a[j],lambda0,lambda1,theta,1,n,delta);

	  // If something enters the eligible set, update eligible set & residuals
	  
	    if (b[l*p+j] !=0) {
	    
	      e1[j] = e2[j] = 1;
	      
	      for (int i=0; i<n; i++) r[i] -= b[l*p+j]*X[j*n+i];
	    
	      a[j] = b[l*p+j];
	    
	      violations++;

	      counter++;
	  }
	}
      
	if (counter==count_max  &&  strcmp(penalty, "adaptive")==0){
	      
	  theta=expectation_approx(b, aa, bb,p,l);
	  
	  delta= threshold( theta, lambda1, lambda0, n, approximate);
	  
	  counter=0;
	
	}

      }
      
      if (violations==0) {

	REAL(loss)[l] = gLoss(r, n);

	break;
      }
    }
  }

  res = cleanupG(a, r, e1, e2, z, thetas, beta, loss, iter, thetas_export);

  return(res);
}
