library(mvtnorm)
#library('latex2exp')
library(xtable)
library(invgamma)
#install.packages("mvtnorm")
#install.packages("xtable")
#install.packages("invgamma")
#install.packages("BayesVarSel")
library(BayesVarSel)
rm(list=ls())
##############
## initial  ##
## settings ##
##############
p<-1000
rho0 <- 0.9
mcsize = 200
REP<-2000
n<-100
rep0 <- rep(0,p)
seq.p <- c(1:p)
I_n <- diag(1,n)
Sig<-rho0^(abs(matrix(1:p,p,p)-t(matrix(1:p,p,p))));
k0<-ceiling(n^(2/3))
Sig2<-1 # variance for y1
a.sigma = b.sigma = 1
##############

################
## simulation ##
################
G1.FNVS <- list()
G1.hy <- list()
FDR<-rep(0,REP)
FOR<-rep(0,REP)
t2 <- rep(NA,REP)
n.var <- rep(0,REP)
n.true <- rep(0,REP)
hat.gamma <- matrix(0,p,REP)
ham <- rep(0,REP)
for(i in 1:REP){
  #i=1
  set.seed(1314+i)
  beta.s <- sample(c(-2,-1,1,2), size = 4, prob = rep(0.25,4), replace = TRUE)
  true.var <- sort(sample(seq.p,4))
  beta = rep0
  beta[true.var] = beta.s
  false.var <- setdiff(seq.p,true.var)
  gamma.true <- rep0
  gamma.true[true.var] <- 1
  X1<-rmvnorm(n,rep(0,p),Sig,method="chol") #By Goh
  y1<-as.numeric(X1%*%beta)+rnorm(n,mean=0,sd=sqrt(Sig2))

Sim.data<-data.frame(y=y1,X=X1)
Sim.data[1:2,]
t1 = Sys.time()
SSVS.sim<-GibbsBvs(formula= y ~ ., data=Sim.data, n.iter=1000, init.model=rep(0,p), n.burnin=100,
time.test = FALSE)
t2[i] <- round(difftime(Sys.time(),t1,units = 'secs'), 3)
result.SSVS<-as.numeric((summary(SSVS.sim)$summary[[3]])=="*")
  G.1.hy <-which(result.SSVS==1)
  G.0.hy = setdiff(seq.p,G.1.hy)
  n.var[i] <- length(G.1.hy)
  FDR[i] <- length(setdiff(G.1.hy, true.var))/n.var[i]
  FOR[i] <- length(setdiff(G.0.hy, false.var))/(p-n.var[i])
  n.true[i] <- as.numeric(all(G.1.hy == true.var))*100
  hat.gamma[G.1.hy,i] <- 1
  ham[i] <- sum(abs(hat.gamma[,i]- gamma.true))
  print(paste0(i,'th Rep',',FDR=',FDR[i],',FOR=',FOR[i],
               ',TRUE=',n.true[i],',Nvar=',n.var[i],
               ',HAM=',ham[i],',Time=',t2[i]))
}

## FDR,FOR and time
result0 <- cbind(FDR,FOR,n.true,n.var,ham,t2)
result1 <- rbind(colMeans(result0), apply(result0,2,sd)/sqrt(REP))
result2 <- data.frame(matrix(result1, nrow = 1, ncol =  12))
names(result2) <- c('FDR.mean','FDR.se','FOR.mean','FOR.se',
                    'n.true.mean','n.true.se','n.var.mean','n.var.se',
                    'ham.mean','ham.se','time.mean','time.se')
result.tex <- xtable(result2,digits = 3) 
print(result.tex)
# setwd('~/Dropbox/Goh/Project 2/33/FNVS_hy')
#save(list = ls(), file = paste0('FNVS_hy_MCMC=',mcsize,'_n=',n,'_p=',p,'_rho=',rho0,'.RData'))
