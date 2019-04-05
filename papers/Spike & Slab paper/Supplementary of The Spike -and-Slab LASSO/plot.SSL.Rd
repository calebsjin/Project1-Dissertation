\name{plot.SSL}
\alias{plot.SSL}
\title{Plot coefficients from a "SSL" object}
\description{Produces a plot of the coefficient paths for a fitted
  \code{"SSL"} object.}
\usage{
\method{plot}{SSL}(x)
}
\arguments{
  \item{x}{Fitted \code{"SSL"} model.}
}
\references{Rockova, V.  and George, E. (2015) The Spike-and-Slab LASSO (In revision)}
\author{Veronika Rockova <vrockova@wharton.upenn.edu>, <Veronika.Rockova@chicagobooth.edu>}
\seealso{\code{\link{SSL}}}
\examples{
	
## Linear regression, where p>n
library(SSL)

n=100
p=1000
X=matrix(rnorm(n*p),n,p)
beta=c(1,2,3,rep(0,p-3))
Y=X[,1]*beta[1]+X[,2]*beta[2]+X[,3]*beta[3]+rnorm(n)
lambda1<-0.1
lambda0<-seq(lambda1,100,length=50)
theta<-0.5


# Separable penalty with fixed theta

result<-SSL(X, Y,penalty="separable",lambda=lambda0,lambda1=lambda1,theta=theta)

plot(result)

}
\keyword{models}
\keyword{regression}
