library(MASS)
library(invgamma)
library(mvtnorm)
library(xtable)
library(ggplot2)
library(reshape2)
library(glmnet)
library(ncvreg)
rm(list=ls())
###################
# Data processing #
###################
load('~/Dropbox/Goh/BRCA.RData')
#load('~/BRCA.RData')
## data processing
BRCA.stu <- scale(BRCA,center = TRUE, scale = TRUE)
name.genes <- colnames(BRCA)
ham.genes <- c('BRCA2','NBR1','NBR2','XRCC2',
               'ATM','CHEK2','BRIP1','PALB2','RAD51C',
               'CDH1','TP53')
BRCA1 <- which(name.genes=='BRCA1')
y1 <- BRCA.stu[,BRCA1]
X1 <- BRCA.stu[,-BRCA1]
name.genes <- name.genes[-BRCA1] # update symbols of genes. 
ham.vars <- name.genes%in%ham.genes
lm.ham.gene <- lm(y1~X1[,ham.vars]-1)
p = dim(X1)[2]
n = length(y1)
rep0 <- rep(0,p)
ksi <- 1.01-0.5*log(n)/log(p)
set.seed(1314)
g.tau <- function(tau0) {
  SVD <- svd(X.r.star)
  d.j <- as.vector(SVD$d)
  z.j <- as.vector(t(SVD$u)%*%y1)
  log.g.tau <- dim(X.r.star)[2]/2*log(tau0) + 0.5*sum(log(1/tau0+d.j^2)) + 
    0.5*(a.sigma+n)*log(sum(y1^2)- sum((d.j*z.j)^2/(d.j^2+1/tau0))+b.sigma)
  return(log.g.tau)
}
G1.BRO <- list()
G1.hy <- list()
k0 <- ceiling(n^(2/3))
a.sigma = b.sigma = 1
var.name <- colnames(X1)
I_n=diag(1,n)
marginal <- rep(NA,k0)
G1 <- list()
##################
#1. Hybrid #####
##################
#Step 1: Set initial values
beta.ini <- cor(X1,y1)
marginal.BRO <- rep(NA,k0)
marginal.hy <- rep(NA,k0)
G1.hy <- list()
d.tXX<-colSums(X1^2)
sum.y2<-sum(y1^2)
tXy<-t(X1)%*%y1
tau_a = 10
for (k in 1:k0) {
  #k<-20
  print(k)
  tau=1000
  I_k <- diag(1,k)
  I_k1 <- diag(1,k+1)
  log.choose <- lchoose(p,k)
  gamma.0 = as.numeric(abs(beta.ini)>=(sort(abs(beta.ini), decreasing = TRUE)[k]))
  G.1 <- which(gamma.0==1)
  G.0 <- which(gamma.0==0)
  marginal.hy[k]=1
  marginal.BRO[k]=0
  while (marginal.BRO[k]<marginal.hy[k]) {
    for(Goh in 1:1000){
      tau.new<-tau
      while(1>0){
        X.r <- X1[,G.1]
        X_r <- X1[,G.0]
        inv.XX.r <- solve(crossprod(X.r)+tau^-1*I_k)
        tX.rX_r<-t(X.r)%*%X_r
        # diag.XHX<-d.tXX[G.0]- diag(t(tX.rX_r)%*%inv.XX.r%*%tX.rX_r)
        diag.XHX<-d.tXX[G.0]- colSums(t(inv.XX.r)%*%tX.rX_r*tX.rX_r)  
        tXX.Xy<-(inv.XX.r%*%tXy[G.1])
        tyHy<-sum.y2-t(tXy[G.1])%*%tXX.Xy
        tXHy<-tXy[G.0]-t(tX.rX_r)%*%tXX.Xy
        d1 <- tau^-1+diag.XHX
        XI.log <- -0.5*(a.sigma+n)*log(rep(tyHy,p-k)-(tXHy)^2/(d1) + b.sigma) - 
          0.5 * log(d1)
        XI <- exp(XI.log)
        i.star <- G.0[which.max(XI.log)]
        G.1.star <- sort(union(G.1,i.star))
        tXX.r.star <- crossprod(X1[,G.1.star]) # tXX[G.1.star, G.1.star]
        tX.star.y <- tXy[G.1.star]
        tXX.inv.XX.r <- tXX.r.star%*%solve(tXX.r.star+tau^-1*I_k1)
        diag.XHX.star <- diag((I_k1 - tXX.inv.XX.r)%*%tXX.r.star)
        tXHy.star <- (I_k1 - tXX.inv.XX.r)%*%tX.star.y
        tyHy.star <- sum.y2 - t(tX.star.y)%*%solve(tXX.r.star+tau^-1*I_k1)%*%tX.star.y
        d2 <- 1/tau - diag.XHX.star
        ETA.log <- -0.5*(a.sigma+n)*log(rep(tyHy.star,k+1) + tXHy.star^2/(d2) + b.sigma) - 
          0.5 * log(d2)
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
    }
    H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k)%*%t(X.r.star)
    marginal.BRO[k] <- -0.5*(a.sigma+n)*log(t(y1)%*%H.r.star%*%y1 + b.sigma) -0.5*k*log(tau) - 
      0.5*determinant(crossprod(X.r.star)+1/tau*I_k,logarithm = TRUE)$modulus[1] - ksi*log.choose
    G1.BRO[[k]] <- G.1
    ############
    ## Hybrid ##
    ############
    C1 = 8000
    marginal.count=rep(NA, C1)
    G.1.tilde<-G.1
    G.0.tilde<-G.0
    G.1.tilde.store=array(NA,dim = c(C1,k))
    for(count in 1:C1){
      X.r <- X1[,G.1.tilde]
      X_r <- X1[,G.0.tilde]
      inv.XX.r <- solve(crossprod(X.r)+tau^-1*I_k)
      tX.rX_r<-t(X.r)%*%X_r
      # diag.XHX<-d.tXX[G.0.tilde]- diag(t(tX.rX_r)%*%inv.XX.r%*%tX.rX_r)
      diag.XHX.star <- d.tXX[G.0.tilde] - colSums(inv.XX.r%*%tX.rX_r*tX.rX_r)
      tXX.Xy<-(inv.XX.r%*%tXy[G.1.tilde])
      tyHy<-sum.y2-t(tXy[G.1.tilde])%*%tXX.Xy
      tXHy<-tXy[G.0.tilde]-t(tX.rX_r)%*%tXX.Xy
      d1 <- tau^-1 + diag.XHX
      XI.log <- (-0.5*(a.sigma+n)*log(rep(tyHy,p-k)-(tXHy)^2/(d1) + b.sigma) - 
        0.5 * log(d1))/tau_a
      XI.log_max <- XI.log - max(XI.log)
      XI <- exp(XI.log_max)
      i.star <- G.0.tilde[sample(x = c(1:length(XI)), size = 1, prob = XI/sum(XI))]
      G.1.star <- sort(union(G.1.tilde,i.star))
      tXX.r.star <- crossprod(X1[,G.1.star])
      tX.star.y <- tXy[G.1.star]
      tXX.inv.XX.r <- tXX.r.star%*%solve(tXX.r.star+tau^-1*I_k1)
      diag.XHX.star <- diag((I_k1 - tXX.inv.XX.r)%*%tXX.r.star)
      tXHy.star <- (I_k1 - tXX.inv.XX.r)%*%tX.star.y
      tyHy.star <- sum.y2 - t(tX.star.y)%*%solve(tXX.r.star+tau^-1*I_k1)%*%tX.star.y
      d2 <- 1/tau - diag.XHX.star
      ETA.log <- (-0.5*(a.sigma+n)*log(rep(tyHy.star,k+1) + tXHy.star^2/(d2) + b.sigma) -
        0.5 * log(d2))/tau_a
      ETA.log_max <- ETA.log - max(ETA.log)
      ETA <- exp(ETA.log_max)
      j.star <- G.1.star[sample(x = c(1:length(ETA)),size = 1, prob = ETA/sum(ETA))]
      G.1.tilde = setdiff(G.1.star,j.star)
      G.1.tilde.store[count,] <- G.1.tilde
      G.0.tilde = setdiff(seq(1,p),G.1.tilde)
      X.r.star=as.matrix(X1[,G.1.tilde])
      H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k)%*%t(X.r.star)
      marginal.count[count] <- -0.5*(a.sigma+n)*log(t(y1)%*%H.r.star%*%y1 + b.sigma) -0.5*k*log(tau) - 
        0.5*determinant(crossprod(X.r.star)+1/tau*I_k,logarithm = TRUE)$modulus - 
        ksi*log.choose # Shiqiang
      if (marginal.BRO[k] < marginal.count[count]) {
        G.1 <- G.1.tilde
        G.0 <- setdiff(seq(1,p),G.1)
        marginal.hy[k] <- marginal.count[count]
        print(marginal.hy[k])
        break
      }
    } # for (count in 1:C1) ends
    if (count == C1) {break}
  } # whie (marginal.hy[k]<marginal.BRO[k]) ends here
  l=which.max(marginal.count)
  G1.hy[[k]] <- G.1.tilde.store[l,]
  marginal.hy[k] = marginal.count[l]
}
choice.hy <- G1.hy[[which.max(marginal.hy)]]
choice.hy.name <- var.name[choice.hy]
lm.hy <- lm(y1~X1[,choice.hy]-1)
hat.beta.hy <- rep(0,p)
hat.beta.hy[choice.hy] <- coef(lm.hy)
predict.hy <- X1%*%hat.beta.hy
fit.MSE.hy <- sqrt(mean((y1-predict.hy)^2))
BIC.hy <- BIC(lm.hy) 
EBIC.hy <- BIC.hy + 2*ksi*lchoose(p,length(choice.hy))
print(choice.hy)
print(choice.hy.name)

##################
#2. local #########
##################
#Step 1: Set initial values
marginal.BRO <- rep(NA,k0)
G1.BRO <- list()
for (k in 1:k0) {
  #k<-1
  print(k)
  tau=1000
  I_k <- diag(1,k)
  I_k1 <- diag(1,k+1)
  log.choose <- lchoose(p,k)
  gamma.0 = as.numeric(abs(beta.ini)>=(sort(abs(beta.ini), decreasing = TRUE)[k]))
  G.1 <- which(gamma.0==1)
  G.0 <- which(gamma.0==0)
  iter=0
  for(Goh in 1:1000){
    tau.new<-tau
    while(1>0){
      X.r <- X1[,G.1]
      X_r <- X1[,G.0]
      inv.XX.r <- solve(crossprod(X.r)+tau^-1*I_k)
      tX.rX_r<-t(X.r)%*%X_r
      diag.XHX<-d.tXX[G.0]- diag(t(tX.rX_r)%*%inv.XX.r%*%tX.rX_r)
      tXX.Xy<-(inv.XX.r%*%tXy[G.1])
      tyHy<-sum.y2-t(tXy[G.1])%*%tXX.Xy
      tXHy<-tXy[G.0]-t(tX.rX_r)%*%tXX.Xy
      d1 <- tau^-1 + diag.XHX
      XI.log <- -0.5*(a.sigma+n)*log(rep(tyHy,p-k)-(tXHy)^2/(d1) + b.sigma) - 
        0.5 * log(d1)
      XI <- exp(XI.log)
      i.star <- G.0[which.max(XI.log)]
      G.1.star <- sort(union(G.1,i.star))
      tXX.r.star <- crossprod(X1[,G.1.star])
      tX.star.y <- tXy[G.1.star]
      tXX.inv.XX.r <- tXX.r.star%*%solve(tXX.r.star+tau^-1*I_k1)
      diag.XHX.star <- diag((I_k1 - tXX.inv.XX.r)%*%tXX.r.star)
      tXHy.star <- (I_k1 - tXX.inv.XX.r)%*%tX.star.y
      tyHy.star <- sum.y2 - t(tX.star.y)%*%solve(tXX.r.star+tau^-1*I_k1)%*%tX.star.y
      ETA.log <- -0.5*(a.sigma+n)*log(rep(tyHy.star,k+1) + tXHy.star^2/(1/tau - diag.XHX.star) + b.sigma) - 
        0.5 * log(1/tau - diag.XHX.star)
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
    iter=iter+1
    if(abs(tau.new-tau)<(0.1)^5){break}
  }
  H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k)%*%t(X.r.star)
  marginal.BRO[k] <- -0.5*(a.sigma+n)*log(t(y1)%*%H.r.star%*%y1 + b.sigma) -0.5*k*log(tau) - 
    0.5*determinant(crossprod(X.r.star)+1/tau*I_k,logarithm = TRUE)$modulus[1] - ksi*log.choose
  G1.BRO[[k]] <- G.1
}
choice.BRO <- G1.BRO[[which.max(marginal.BRO)]]
choice.BRO.name <- var.name[choice.BRO]
lm.BRO <- lm(y1~X1[,choice.BRO]-1)
hat.beta.BRO <- rep0
hat.beta.BRO[choice.BRO] <- coef(lm.BRO)
predict.BRO <- X1%*%hat.beta.BRO
fit.MSE.BRO <- sqrt(mean((y1-predict.BRO)^2))
BIC.BRO <- BIC(lm.BRO) 
EBIC.BRO <- BIC.BRO + 2*ksi*lchoose(p,length(choice.BRO))
print(choice.BRO)
print(choice.BRO.name)

#########################
## Freq prediction ######
#########################
############
#1. LASSO ##
############

fit.LASSO<-glmnet(X1,y1,intercept=FALSE,family="gaussian",alpha=1) #alpha=1 LASSO, alpha=0 Ridge 
hat.BETA.LASSO <- as.matrix(fit.LASSO$beta)
n.lambda <- length(fit.LASSO$lambda)
EBIC.LASSO0 <- rep(NA,n.lambda)
for (i0 in 1:n.lambda) {
  choice.lasso=which(hat.BETA.LASSO[,i0]!=0)
  len.choice <- length(choice.lasso)
  if (len.choice>=1 && len.choice<=k0) {
    EBIC.LASSO0[i0] <- BIC(lm(y1~X1[,choice.lasso]-1)) + 2*ksi*lchoose(p,len.choice) 
  }
}

choice.LASSO <- which(hat.BETA.LASSO[,which.min(EBIC.LASSO0)]!=0)
NSP.LASSO <- length(choice.LASSO)
hat.beta.LASSO <- rep0
lm.LASSO <- lm(y1~X1[,choice.LASSO]-1)
hat.beta.LASSO[choice.LASSO] <- coef(lm.LASSO)
predict.LASSO <- X1%*%hat.beta.LASSO
fit.MSE.LASSO <- sqrt(mean((y1-predict.LASSO)^2))
BIC.LASSO <- BIC(lm.LASSO)
EBIC.LASSO <- BIC.LASSO + 2*ksi*lchoose(p,length(choice.LASSO))
###################################
#2. SCAD ##########################
###################################

fit.SCAD<-ncvreg(X1,y1,family="gaussian",alpha=1,penalty="SCAD") #10 folds CV #
hat.BETA.SCAD <- as.matrix(fit.SCAD$beta)[-1,]
n.lambda <- length(fit.SCAD$lambda)
EBIC.SCAD0 <- rep(NA,n.lambda)
for (i0 in 1:n.lambda) {
  choice.scad=which(hat.BETA.SCAD[,i0]!=0)
  len.choice <- length(choice.scad)
  if (len.choice>=1 && len.choice<=k0) {
    EBIC.SCAD0[i0] <- BIC(lm(y1~X1[,choice.scad]-1)) + 2*ksi*lchoose(p,len.choice) 
  }
}
choice.SCAD <- which(hat.BETA.SCAD[,which.min(EBIC.SCAD0)]!=0)
NSP.SCAD <- length(choice.SCAD)
hat.beta.SCAD <- rep0
lm.SCAD <- lm(y1~X1[,choice.SCAD]-1)
hat.beta.SCAD[choice.SCAD] <- coef(lm.SCAD)
predict.SCAD <- X1%*%hat.beta.SCAD
fit.MSE.SCAD <- sqrt(mean((y1-predict.SCAD)^2))
BIC.SCAD <- BIC(lm(y1~X1[,choice.SCAD]-1))
EBIC.SCAD <- BIC.SCAD + 2*ksi*lchoose(p,length(choice.SCAD)) 
####################################
#3. MCP ############################
####################################
fit.MCP<-ncvreg(X1,y1,family="gaussian",gamma=10,alpha=1,penalty="MCP")
hat.BETA.MCP <- as.matrix(fit.MCP$beta)[-1,]
n.lambda <- length(fit.MCP$lambda)
EBIC.MCP0 <- rep(NA,n.lambda)
for (i0 in 1:n.lambda) {
  choice.mcp=which(hat.BETA.MCP[,i0]!=0)
  len.choice <- length(choice.mcp)
  if (len.choice>=1 && len.choice<=k0) {
    EBIC.MCP0[i0] <- BIC(lm(y1~X1[,choice.mcp]-1)) + 2*ksi*lchoose(p,len.choice) 
  }
}
choice.MCP <- which(hat.BETA.MCP[,which.min(EBIC.MCP0)]!=0)
NSP.MCP <- length(choice.MCP)
hat.beta.MCP <- rep0
lm.MCP <- lm(y1~X1[,choice.MCP]-1)
hat.beta.MCP[choice.MCP] <- coef(lm.MCP)
predict.MCP <- X1%*%hat.beta.MCP
fit.MSE.MCP <- sqrt(mean((y1-predict.MCP)^2))
BIC.MCP <- BIC(lm(y1~X1[,choice.MCP]-1))
EBIC.MCP <- BIC.MCP + 2*ksi*lchoose(p,length(choice.MCP)) 
###################################
#4. ENET #########################
###################################

fit.ENET<-glmnet(X1,y1,intercept=FALSE,family="gaussian",alpha=0.5) #alpha=0.5 ENET, alpha=0 Ridge 
hat.BETA.ENET <- as.matrix(fit.ENET$beta)
n.lambda <- length(fit.ENET$lambda)
EBIC.ENET0 <- rep(NA,n.lambda)
for (i0 in 1:n.lambda) {
  choice.enet=which(hat.BETA.ENET[,i0]!=0)
  len.choice <- length(choice.enet)
  if (len.choice>=1 && len.choice<=k0) {
    EBIC.ENET0[i0] <- BIC(lm(y1~X1[,choice.enet]-1)) + 2*ksi*lchoose(p,len.choice) 
  }
}
choice.ENET <- which(hat.BETA.ENET[,which.min(EBIC.ENET0)]!=0)
NSP.ENET <- length(choice.ENET)
hat.beta.ENET <- rep0
lm.ENET <- lm(y1~X1[,choice.ENET]-1)
hat.beta.ENET[choice.ENET] <- coef(lm.ENET)
predict.ENET <- X1%*%hat.beta.ENET
fit.MSE.ENET <- sqrt(mean((y1-predict.ENET)^2))
BIC.ENET <- BIC(lm(y1~X1[,choice.ENET]-1))
EBIC.ENET <- BIC.ENET + 2*ksi*lchoose(p,length(choice.ENET)) 


EBIC.ALL <- cbind(EBIC.BRO,EBIC.hy,EBIC.SCAD,EBIC.MCP,EBIC.ENET,EBIC.LASSO)
BIC.ALL <- cbind(BIC.BRO,BIC.hy,BIC.SCAD,BIC.MCP,BIC.ENET,BIC.LASSO)
BIC.star <- data.frame(rbind(BIC.ALL,EBIC.ALL),row.names = c('BIC','EBIC'))
names(BIC.star) = c('BRO','hy','SCAD','MCP','ENET','LASSO')
##########
### CV ###
##########
n1 <- ceiling(n*0.7)
REP = 500
MSE.BRO <- rep(NA,REP)
MSE.hy <- rep(NA,REP)
MSE.SCAD <- rep(NA,REP)
MSE.MCP <- rep(NA,REP)
MSE.ENET <- rep(NA,REP)
MSE.LASSO <- rep(NA,REP)
for (i in 1:REP) {
  set.seed(1314+i)
  index <- sample(c(1:n))
  X_shuffle <- X1[index,]
  y_shuffle <- y1[index]
  X2 <- X_shuffle[1:n1,] # training
  y2 <- y_shuffle[1:n1,] # training
  X3 <-X_shuffle[-(1:n1),]  # testing
  y3 <- y_shuffle[-(1:n1),] # testing
  ## BRO ###
  beta.BRO <- coef(lm(y2~X2[,choice.BRO]))
  pred.BRO <- cbind(rep(1,n1),X3[,choice.BRO])%*%beta.BRO
  MSE.BRO[i] <- sqrt(sum((y3-pred.BRO)^2)/(n1))
  ## hy ###
  beta.hy <- coef(lm(y2~X2[,choice.hy]))
  pred.hy <- cbind(rep(1,n1),X3[,choice.hy])%*%beta.hy
  MSE.hy[i] <- sqrt(sum((y3-pred.hy)^2)/(n1))
  ## SCAD ###
  beta.SCAD <- coef(lm(y2~X2[,choice.SCAD]))
  pred.SCAD <- cbind(rep(1,n1),X3[,choice.SCAD])%*%beta.SCAD
  MSE.SCAD[i] <- sqrt(sum((y3-pred.SCAD)^2)/(n1))
  ## MCP ###
  beta.MCP <- coef(lm(y2~X2[,choice.MCP]))
  pred.MCP <- cbind(rep(1,n1),X3[,choice.MCP])%*%beta.MCP
  MSE.MCP[i] <- sqrt(sum((y3-pred.MCP)^2)/(n1))
  ## ENET ###
  beta.ENET <- coef(lm(y2~X2[,choice.ENET]))
  pred.ENET <- cbind(rep(1,n1),X3[,choice.ENET])%*%beta.ENET
  MSE.ENET[i] <- sqrt(sum((y3-pred.ENET)^2)/(n1))
  ## LASSO ###
  beta.LASSO <- coef(lm(y2~X2[,choice.LASSO]))
  pred.LASSO <- cbind(rep(1,n1),X3[,choice.LASSO])%*%beta.LASSO
  MSE.LASSO[i] <- sqrt(sum((y3-pred.LASSO)^2)/(n1))
}
NSP.list <- list(choice.BRO,choice.hy,choice.SCAD,choice.MCP,choice.ENET,choice.LASSO)
NSP <- unlist(lapply(NSP.list,length))
MSE.pred <- colMeans(cbind(MSE.BRO,MSE.hy,MSE.SCAD,MSE.MCP,MSE.ENET,MSE.LASSO))
MSE.fit <- cbind(fit.MSE.BRO,fit.MSE.hy,fit.MSE.SCAD,fit.MSE.MCP,fit.MSE.ENET,fit.MSE.LASSO)
result <- data.frame(cbind(NSP,MSE.pred,t(MSE.fit),t(BIC.star)),
                     row.names = c('FNVS','FNVS-hy','SCAD','MCP','ENET','LASSO'))
names(result) <- c('NSP','MSE_Pred','MSE_fit','BIC','EBIC')

result.tex <- xtable(result,digits = 2)
result.tex
result
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
## save variables ##
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#

mem.size=list()
kk=0
for (i in ls()) {
  kk=kk+1
  mem.size[[kk]]=object.size(get(i))
}
var.ls <- ls()[unlist(mem.size)<1024*1024*8]
save(list = var.ls,file = paste0('Ana_BRCA.RData'))
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
## ggplot ##
gamma.BRO <- rep0
gamma.BRO[choice.BRO]=1
gamma.hy <- rep0
gamma.hy[choice.hy]=1
gamma.SCAD <- rep0
gamma.SCAD[choice.SCAD]=1
gamma.MCP <- rep0
gamma.MCP[choice.MCP]=1
gamma.ENET <- rep0
gamma.ENET[choice.ENET]=1
gamma.LASSO <- rep0
gamma.LASSO[choice.LASSO]=1


union.gamma <- colSums(rbind(gamma.BRO,gamma.hy,gamma.SCAD,gamma.MCP,gamma.ENET,gamma.LASSO))
union.nonzero <- which(union.gamma!=0)
all.BETA = rbind(hat.beta.BRO,hat.beta.hy,hat.beta.SCAD,hat.beta.MCP,hat.beta.ENET,hat.beta.LASSO)
all.beta <- all.BETA[,union.nonzero]
tag.metohd <- c('Local','Global','SCAD','MCP','ENET','LASSO')
Method <- factor(tag.metohd,levels = tag.metohd[6:1])
data.beta<- data.frame(Method,all.beta)
names(data.beta) <- c('Method',var.name[union.nonzero])
data.heat <- melt(data.beta,id = 'Method',value.name = 'Coefficient')
p.sig = matrix(NA,6,length(union.nonzero))
p.sig[5,27]='X'

heat.plot <- ggplot(data.heat, aes(x = variable, y = Method, fill = Coefficient)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("cyan", "white", "red"), 
                       values = scales::rescale(c(min(data.heat$Coefficient), 0, max(data.heat$Coefficient)))) + 
  xlab('Genes') + ylab('Methods') + 
  theme(axis.text.x = element_text(face = 'bold',size=5, angle=90)) + 
  theme(axis.text.y = element_text(face = 'bold',size=10)) + 
  theme(axis.title.x.bottom = element_text(face = 'bold',size=15)) + 
  theme(axis.title.y.left = element_text(face = 'bold',size=15)) + 
  geom_hline(yintercept = seq(0.5,7.5,1),colour='grey80') +
  geom_text(aes(label=as.vector(p.sig)), fontface = 'bold', size = 3)
heat.plot
ggsave('Heatmap.pdf', plot = heat.plot,dpi = 300, height = 6, width = 10)

marginal.data <- data.frame(k = 1:k0, Local = marginal.BRO, Global = marginal.hy)
marginal.melt = melt(data = marginal.data,id.vars = 'k', variable.name = 'Method')
log.marginal <- ggplot(marginal.melt) + 
  geom_line(aes(x = k, y=value, color = Method), size = 0.5) + 
  geom_point(aes(x = k, y=value, color = Method,shape=Method), size = 1) + 
  ylab('Log Marginal') + scale_x_discrete(limits = seq(1,k0,10)) +
  scale_linetype_manual(values=c("solid","dotted"))
log.marginal
ggsave('Log_marginal.pdf',plot = log.marginal,dpi = 300, height = 6, width = 10)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
xtable(lm.BRO)
xtable(lm.hy)
xtable(lm.SCAD)
xtable(lm.MCP)
xtable(lm.ENET)
xtable(lm.LASSO)
xtable(lm.ham.gene)

summary(lm.ham.gene)
summary(lm.BRO)
summary(lm.hy)
summary(lm.SCAD)
summary(lm.MCP)
summary(lm.ENET)
summary(lm.LASSO)

## VIF ##
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
hy.vars <- X1[,choice.hy]
lm.dat<-data.frame(y1,hy.vars)
form.in<-paste('y1 ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)