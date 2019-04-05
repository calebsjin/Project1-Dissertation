\name{SSL}
\alias{SSL}
\title{The Spike-and-Slab LASSO}
\description{Spike-and-Slab LASSO is a spike-and-slab refinement of the LASSO procedure, using a mixture of Laplace priors indexed by  \code{lambda0} (spike) and \code{lambda1} (slab). 
	
	The \code{SSL} procedure fits coefficients paths for Spike-and-Slab LASSO-penalized
  linear regression models over a grid of values for the regularization
  parameter \code{lambda_0}. The code has been adapted from the \code{ncvreg} package (Breheny and Huang, 2011). }
\usage{
	SSL(Y, X, penalty=c("separable","adaptive"), lambda0, lambda1, theta, a, b, max.iter, counter, approximate=FALSE, warn=FALSE)
	
	
}
\arguments{
  \item{Y}{Vector of continuous responses (n x 1). The  responses will be centered by default.}
  \item{X}{The design matrix (n x p), without an intercept.  \code{SSL}
  standardizes the data  by default (mean zero and variance \code{n}).}
  \item{penalty}{The penalty to be applied to the model.  Either "separable"
    (with a fixed \code{theta}),  "adaptive" (with a random \code{theta}, where \code{theta~ B(a,p)}).}
  \item{lambda0}{Spike penalty parameters (L x 1). Either a numeric value for a single run (L=1) or a sequence of increasing values for dynamic posterior exploration.}
  \item{lambda1}{Slab variance parameter. Needs to be greater than \code{lambda0}.}
  \item{theta}{Prior mixing proportion. For "separable" penalty, this value is fixed. For "adaptive" penalty, this value is used as a starting value. }
  \item{a}{Hyperparameter of the beta prior \code{B(a,b)} for the adaptive penalty (default \code{a=1})}
  \item{b}{Hyperparameter of the beta prior \code{B(a,b)} for the adaptive penalty (default \code{b=p})}
  \item{max.iter}{Optional. Maximum number of iterations.  Default is 1000.}
  \item{counter}{Optional. Applicable only for the adaptive penalty. Determines how often the parameter \code{theta} is updated throughout the cycles of coordinate ascent. Default is 10.}
  \item{approximate}{TRUE if the selection threshold should be approximated  by
  	 \deqn{ \Delta~ \sqrt{-2n\log[p^\star(0)]}+lambda_1}
  	 
  	 Default value is FALSE.
 	 }	 
  \item{warn}{TRUE if warnings should be printed; FALSE by default}
}
\details{
  The sequence of models indexed by the regularization parameter
  \code{lambda_0} is fitted using a coordinate descent algorithm. The algorithm uses 
  screening rules for discarding irrelevant predictors along the lines of Breheny (2011).
  
   }
\value{
  An object with S3 class \code{"SSL"} containing:
  \item{beta}{The fitted matrix of coefficients (p x L).  The number of rows is
    equal to the number of coefficients \code{p}, and the number of columns is
    equal to \code{L} (the length of \code{lambda_0}).}
  \item{iter}{A vector of length \code{L} containing the number
    of iterations until convergence at each value of \code{lambda_0}.}
  \item{lambda_0}{The sequence of regularization parameter values in the
    path.}
  \item{penalty}{Same as above.}
  \item{thetas}{A vector of length \code{L} containing the hyper-parameter values \code{theta} (the same as \code{theta} for "separable" penalty)}
  \item{select}{A (p x L) binary matrix indicating which variables were selected along the solution path}
  \item{model}{A single model chosen after the stabilization of the regularization path.}
 }
\references{Rockova, V. and George, E.I. (2016) The Spike-and-Slab LASSO. (In revision)}
\author{Veronika Rockova <vrockova@wharton.upenn.edu>, <Veronika.Rockova@chicagobooth.edu>}
\seealso{\code{\link{plot.SSL}}}
\examples{
	
	
## Linear regression, where p>n

library(SSL)

library(mvtnorm)


p<-1000

n<-100

lambda1<-1     # slab penalty for Spike-and-Slab LASSO

lambda0<-seq(lambda1,50,length=20) # slab penalties for Spike-and-Slab LASSO

Sigma<-matrix(0.6,p,p)

diag(Sigma)<-1

X<-rmvnorm(n,numeric(p),Sigma)
        
beta=c(1,2,3,rep(0,p-3))

Y=X[,1]*beta[1]+X[,2]*beta[2]+X[,3]*beta[3]+rnorm(n)

# Oracle SSL

result1<-SSL(X, Y,penalty="separable",lambda=lambda0,lambda1=lambda1,theta=3/p,warn=FALSE)
 
plot(result1)

# Adaptive SSL 

result2<-SSL(X, Y,penalty="adaptive",lambda=lambda0,lambda1=lambda1,theta=0.5,a=1,b=p,counter=10,warn=FALSE)

plot(result2)


}
