library(mvtnorm)
#library('latex2exp')
library(xtable)
library(invgamma)
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
## function ##
##############
g.tau <- function(tau0) {
  SVD <- svd(X.r.star)
  d.j <- as.vector(SVD$d)
  z.j <- as.vector(t(SVD$u)%*%y1)
  log.g.tau <- dim(X.r.star)[2]/2*log(tau0) + 0.5*sum(log(1/tau0+d.j^2)) + 
    0.5*(a.sigma+n)*log(sum(y1^2)- sum((d.j*z.j)^2/(d.j^2+1/tau0))+b.sigma)
  return(log.g.tau)
}
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
  ###############################
  #Step 1: Set initial values
  ###############################
  beta.ini= as.numeric(cor(y1,X1))/diag(var(X1))
  marginal.FNVS <- rep(NA,k0)
  marginal.hy <- rep(NA,k0)
  G1.hy <- list()
  tXX <-  crossprod(X1)
  d.tXX<-diag(tXX)
  sum.y2<-sum(y1^2)
  tXy<-t(X1)%*%y1

t1 = Sys.time()
#################################
##BEGIN: Bounded subset size
#################################
  for (k in 1:k0) { #For loop for k<=k0
    #k<-1
    tau=1000
    I_k <- diag(1,k)
    I_k1 <- diag(1,k+1)
    log.choose <- lchoose(p,k)
    gamma.0 = as.numeric(abs(beta.ini)>=(sort(abs(beta.ini),decreasing = TRUE)[k]))
    G.1 <- which(gamma.0==1)
    G.0 <- which(gamma.0==0)
    marginal.hy[k]=1
    marginal.FNVS[k]=0
########################################
#BEGIN: Hybrid Search    ###############
########################################

    while (marginal.FNVS[k]<marginal.hy[k]) {

########################################
#1. Deterministic Search ###############
########################################
      for(Goh in 1:1000){
        tau.new<-tau
        while(1>0){
          X.r <- X1[,G.1]
          X_r <- X1[,G.0]
          tX.rX_r<-t(X.r)%*%X_r

          #inv.XX.r <- solve(crossprod(X.r)+1/tau*I_k)
          #diag.XHX<-d.tXX[G.0]- diag(t(tX.rX_r)%*%inv.XX.r%*%tX.rX_r)

          tXX.r.tau<-(crossprod(X.r)+1/tau*I_k)
          Decomp<-eigen(tXX.r.tau,symmetric=TRUE)
if(k==1){half.tXX.r.tau<-Decomp$vectors*(1/sqrt(Decomp$values))}else{
          half.tXX.r.tau<-Decomp$vectors%*%diag(1/sqrt(Decomp$values))}
          half.XHX<-crossprod(half.tXX.r.tau,tX.rX_r)
          diag.XHX<-d.tXX[G.0]-colSums(half.XHX^2)

          #tXX.Xy<-(inv.XX.r%*%tXy[G.1])
          #tyHy<-sum.y2-t(tXy[G.1])%*%tXX.Xy
	    half.Hy<-crossprod(half.tXX.r.tau,tXy[G.1])
          tyHy<-sum.y2-sum(half.Hy^2)

          #tXHy<-tXy[G.0]-t(tX.rX_r)%*%tXX.Xy
          tXHy<-as.numeric(tXy[G.0]-crossprod(half.XHX,half.Hy))

          XI.log <- -0.5*(a.sigma+n)*log(tyHy-(tXHy)^2/(1/tau+diag.XHX) + b.sigma) - 0.5 * log(1/tau + diag.XHX)
          XI <- exp(XI.log)
          i.star <- G.0[which.max(XI.log)]
          G.1.star <- sort(union(G.1,i.star))
          X.r.star <- X1[,G.1.star]
         # H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k1)%*%t(X.r.star) # Shiqiang
         # diag.XHX.star <- diag(t(X.r.star)%*%H.r.star%*%X.r.star)  # Shiqiang
         #ETA.log <- -0.5*(a.sigma+n)*log(rep(t(y1)%*%H.r.star%*%y1,k+1) + (t(X.r.star)%*%H.r.star%*%y1)^2/(1/tau-diag.XHX.star) + b.sigma)-0.5 * log(1/tau - diag.XHX.star)  # Shiqiang

          tXX.r.tau.star<-(crossprod(X.r.star)+1/tau*I_k1)
          Decomp.star<-eigen(tXX.r.tau.star,symmetric=TRUE)
          half.tXX.r.tau.star<-Decomp.star$vectors%*%diag(1/sqrt(Decomp.star$values))
          half.XHX.star<-crossprod(half.tXX.r.tau.star,crossprod(X.r.star))
          diag.XHX.star<-d.tXX[G.1.star]-colSums(half.XHX.star^2)
	    half.Hy.star<-crossprod(half.tXX.r.tau.star,tXy[G.1.star])
          tyHy.star<-sum.y2-sum(half.Hy.star^2)
          tXHy.star<-as.numeric(tXy[G.1.star]-crossprod(half.XHX.star,half.Hy.star))
          ETA.log <- -0.5*(a.sigma+n)*log(tyHy.star + (tXHy.star)^2/(1/tau-diag.XHX.star) + b.sigma)-0.5 * log(1/tau - diag.XHX.star) 

          ETA <- exp(ETA.log)
          j.star <- G.1.star[which.max(ETA.log)]
          if (j.star == i.star) {
            break
          } else {
            G.1 = setdiff(G.1.star,j.star)
            G.0 = setdiff(seq(1,p),G.1)
          }
        }
        X.r.star=as.matrix(X1[,G.1])
        tau<-optim(1,g.tau,method="Brent",lower=0,upper=10^3)$par
        if(abs(tau.new-tau)<(0.1)^5){break}
      } #END of for loop in goh
########################################
#END: Deterministic Search #############
########################################

##################################################

      tXy.g1<-as.matrix(tXy[G.1])
      yy.star <- sum.y2-as.numeric(t(tXy.g1)%*%(solve(crossprod(X.r.star)+1/tau*I_k)%*%tXy.g1))  # Shiqiang
      marginal.FNVS[k] <- -0.5*(a.sigma+n)*log(yy.star + b.sigma) -0.5*k*log(tau) - 
        0.5*(determinant(crossprod(X.r.star)+1/tau*I_k,logarithm = TRUE)$modulus)[1] - log.choose # Shiqiang: & put lchoose in the outside of for loop
      G1.FNVS[[k]] <- G.1

##########################
## 2. Stochastic Search ##
##########################
      
      marginal.count=rep(NA, mcsize)
      G.1.tilde<-G.1
      G.0.tilde<-G.0
      G.1.tilde.store=array(NA,dim = c(mcsize,k))
      for(count in 1:mcsize){
        X.r <- X1[,G.1.tilde]
        X_r <- X1[,G.0.tilde]
       # inv.XX.r <- solve(crossprod(X.r)+1/tau*I_k)
       # tX.rX_r<-t(X.r)%*%X_r
       # diag.XHX<-d.tXX[G.0.tilde]- diag(t(tX.rX_r)%*%inv.XX.r%*%tX.rX_r)
       # tXX.Xy<-(inv.XX.r%*%tXy[G.1.tilde])
       # tyHy<-sum.y2-t(tXy[G.1.tilde])%*%tXX.Xy
       # tXHy<-tXy[G.0.tilde]-t(tX.rX_r)%*%tXX.Xy
       # XI.log <- -0.5*(a.sigma+n)*log(rep(tyHy,p-k)-(tXHy)^2/(1/tau+diag.XHX) + b.sigma) - 0.5 * log(1/tau + diag.XHX)
       # XI <- exp(XI.log)

          tX.rX_r<-t(X.r)%*%X_r
          tXX.r.tau<-(crossprod(X.r)+1/tau*I_k)
          Decomp<-eigen(tXX.r.tau,symmetric=TRUE)

if(k==1){half.tXX.r.tau<-Decomp$vectors*(1/sqrt(Decomp$values))}else{
          half.tXX.r.tau<-Decomp$vectors%*%diag(1/sqrt(Decomp$values))}

          half.XHX<-crossprod(half.tXX.r.tau,tX.rX_r)
          diag.XHX<-d.tXX[G.0.tilde]-colSums(half.XHX^2)
	    half.Hy<-crossprod(half.tXX.r.tau,tXy[G.1.tilde])
          tyHy<-sum.y2-sum(half.Hy^2)
          #tXHy<-tXy[G.0.tilde]-t(tX.rX_r)%*%tXX.Xy
          tXHy<-as.numeric(tXy[G.0.tilde]-crossprod(half.XHX,half.Hy))
          XI.log <- -0.5*(a.sigma+n)*log(tyHy-(tXHy)^2/(1/tau+diag.XHX) + b.sigma) - 0.5 * log(1/tau + diag.XHX)
          XI <- exp(XI.log-max(XI.log))

        i.star <- G.0.tilde[sample(x = c(1:length(XI)),size = 1, prob = XI/sum(XI))]
        G.1.star <- sort(union(G.1.tilde,i.star))
        X.r.star <- X1[,G.1.star]

#        H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k1)%*%t(X.r.star)# Shiqiang
#        diag.XHX.star <- diag(t(X.r.star)%*%H.r.star%*%X.r.star) # Shiqiang
#        ETA.log <- -0.5*(a.sigma+n)*log(rep(t(y1)%*%H.r.star%*%y1,k+1) + 
#                                          (t(X.r.star)%*%H.r.star%*%y1)^2/(1/tau-diag.XHX.star) + b.sigma) -
#          0.5 * log(1/tau - diag.XHX.star) # Shiqiang
#        ETA <- exp(ETA.log)
          tXX.r.tau.star<-(crossprod(X.r.star)+1/tau*I_k1)
          Decomp.star<-eigen(tXX.r.tau.star,symmetric=TRUE)
          half.tXX.r.tau.star<-Decomp.star$vectors%*%diag(1/sqrt(Decomp.star$values))
          half.XHX.star<-crossprod(half.tXX.r.tau.star,crossprod(X.r.star))
          diag.XHX.star<-d.tXX[G.1.star]-colSums(half.XHX.star^2)
	    half.Hy.star<-crossprod(half.tXX.r.tau.star,tXy[G.1.star])
          tyHy.star<-sum.y2-sum(half.Hy.star^2)
          tXHy.star<-as.numeric(tXy[G.1.star]-crossprod(half.XHX.star,half.Hy.star))
          ETA.log <- -0.5*(a.sigma+n)*log(tyHy.star + (tXHy.star)^2/(1/tau-diag.XHX.star) + b.sigma)-0.5 * log(1/tau - diag.XHX.star)
          ETA <- exp(ETA.log-max(ETA.log))
        j.star <- G.1.star[sample(x = c(1:length(ETA)),size = 1, prob = ETA/sum(ETA) )]
        G.1.tilde = setdiff(G.1.star,j.star)
        G.1.tilde.store[count,] <- G.1.tilde
        G.0.tilde = setdiff(seq(1,p),G.1.tilde)
        X.r.star=as.matrix(X1[,G.1.tilde])

        #H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k)%*%t(X.r.star)
        #marginal.count[count] <- -0.5*(a.sigma+n)*log(t(y1)%*%H.r.star%*%y1 + b.sigma) -0.5*k*log(tau) - 
        #  0.5*determinant(crossprod(X.r.star)+1/tau*I_k,logarithm = TRUE)$modulus - log.choose # Shiqiang
          tXy.g1<-as.matrix(tXy[G.1.tilde])
          yy.star <- sum.y2-as.numeric(t(tXy.g1)%*%(solve(crossprod(X.r.star)+1/tau*I_k)%*%tXy.g1))  # Shiqiang
          marginal.count[count] <- -0.5*(a.sigma+n)*log(yy.star + b.sigma) -0.5*k*log(tau) - 
            0.5*(determinant(crossprod(X.r.star)+1/tau*I_k,logarithm = TRUE)$modulus)[1] - log.choose # Shiqiang: & put lchoose in the outside of for loop
 

        if (marginal.FNVS[k] < marginal.count[count]) {
          G.1 <- G.1.tilde
          G.0 <- setdiff(seq(1,p),G.1)
          marginal.hy[k] <- marginal.count[count]
          # print(marginal.hy[k])
          break        }
      } # for (count in 1:mcsize) ends
###################################################
###########################
##END: Stochastic Search ##
###########################
      if (count == mcsize) {break}
    } # whie (marginal.hy[k]<marginal.FNVS[k]) ends here
###########################
##END: Hybrid Search     ##
###########################
    l=which.max(marginal.count)
    G1.hy[[k]] <- G.1.tilde.store[l,]
    marginal.hy[k] = marginal.count[l]
  } # for (k in 1:k0) ends here

#################################
##END: Bounded subset size
#################################
  t2[i] <- round(difftime(Sys.time(),t1,units = 'secs'), 3)
  G.1.hy <- G1.hy[[which.max(marginal.hy)]]
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
