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
# load('~/Dropbox/Goh/BRCA.RData')
load('~/BRCA.RData')
## data processing
BRCA.stu <- scale(BRCA,center = TRUE, scale = TRUE)
name.genes <- colnames(BRCA)
ham.genes <- c('BRCA2','NBR1','NBR2','XRCC2',
               'ATM','CHEK2','BRIP1','PALB2','RAD51C',
               'CDH1','TP53','LDLRAD1', 'SLC19A1', 
               'FGFBP3', 'CASP5', 'MMAB', 'SLC16A6',
               'NBS1','RAD50','RAD51D','BARD1',
               'CASP8', 'CTLA4', 'CYP19A1', 'FGFR2',
               'H19', 'LSP1', 'MAP3K1', 'MRE11A',
               'NBN', 'RAD51','TERT','MSH6','PMS2',
               'SEC23B','NF1')
BRCA1 <- which(name.genes=='BRCA1')
y1 <- BRCA.stu[,BRCA1]
X0 <- BRCA.stu[,-BRCA1]
name.genes <- name.genes[-BRCA1] # update symbols of genes. 
mcsize = 1000
p=5000
abs.cor <- abs(cor(y1,X0))
g=sort(abs.cor,decreasing = TRUE)[p]
X1 <- X0[,which(abs.cor>=g)]
name.genes.X1 <- colnames(X1)
sum(ham.genes%in%name.genes.X1)
n = length(y1)
k0 <- ceiling(n^(2/3))
rep0 <- rep(0,p)
seq.p <- c(1:p)
I_n <- diag(1,n)
a.sigma = b.sigma = 1
G1.FNVS <- list()
G1.hy <- list()
tau = (log(p))^2
beta.ini= as.numeric(cor(y1,X1))/diag(var(X1))
marginal.FNVS <- rep(NA,k0)
marginal.hy <- rep(NA,k0)
G1.hy <- list()
tXX <-  crossprod(X1)
d.tXX<-diag(tXX)
sum.y2<-sum(y1^2)
tXy<-t(X1)%*%y1
for (k in 1:k0) { #For loop for k<=k0
  #k<-1
  print(k)
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
    #for(Goh in 1:1000){
    #tau.new<-tau
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
      ETA.log <- -0.5*(a.sigma+n)*log(tyHy.star + (tXHy.star)^2/(1/tau-diag.XHX.star) + b.sigma)-
        0.5 * log(1/tau - diag.XHX.star) 
      
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
    # tau<-optim(1,g.tau,method="Brent",lower=0,upper=10^3)$par
    # if(abs(tau.new-tau)<(0.1)^5){break}
    #} #END of for loop in goh
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
      XI.log <- -0.5*(a.sigma+n)*log(tyHy-(tXHy)^2/(1/tau+diag.XHX) + b.sigma) - 
        0.5 * log(1/tau + diag.XHX)
      xi.log1 <- sort(XI.log,decreasing = TRUE)[1:2]
      if (count == 1) {
        if (xi.log1[1]-xi.log1[2]>log(2)) {
          tem = (xi.log1[2] - xi.log1[1])/log(0.5)
        } else {
          tem = 1
        }
      }
      XI.log1 <- XI.log/tem
      XI <- exp(XI.log1-max(XI.log1))
      # plot(XI/sum(XI))
      i.star <- G.0.tilde[sample(x = c(1:length(XI)),size = 1, prob = XI/sum(XI))]
      G.1.star <- sort(union(G.1.tilde,i.star)) 
      X.r.star <- X1[,G.1.star]
      # H.r.star <- I_n-X.r.star%*%solve(crossprod(X.r.star)+1/tau*I_k1)%*%t(X.r.star)# Shiqiang
      # diag.XHX.star <- diag(t(X.r.star)%*%H.r.star%*%X.r.star) # Shiqiang
      # ETA.log <- -0.5*(a.sigma+n)*log(rep(t(y1)%*%H.r.star%*%y1,k+1) +
      #                                   (t(X.r.star)%*%H.r.star%*%y1)^2/(1/tau-diag.XHX.star) + b.sigma) -
      #   0.5 * log(1/tau - diag.XHX.star) # Shiqiang
      # ETA <- exp(ETA.log)
      tXX.r.tau.star<-(crossprod(X.r.star)+1/tau*I_k1)
      Decomp.star<-eigen(tXX.r.tau.star,symmetric=TRUE)
      half.tXX.r.tau.star<-Decomp.star$vectors%*%diag(1/sqrt(Decomp.star$values))
      half.XHX.star<-crossprod(half.tXX.r.tau.star,crossprod(X.r.star))
      diag.XHX.star<-d.tXX[G.1.star]-colSums(half.XHX.star^2)
      half.Hy.star<-crossprod(half.tXX.r.tau.star,tXy[G.1.star])
      tyHy.star<-sum.y2-sum(half.Hy.star^2)
      tXHy.star<-as.numeric(tXy[G.1.star]-crossprod(half.XHX.star,half.Hy.star))
      ETA.log <- (-0.5*(a.sigma+n)*log(tyHy.star + (tXHy.star)^2/(1/tau-diag.XHX.star) + b.sigma)-
                    0.5 * log(1/tau - diag.XHX.star))/tem
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
        # print(count)
        G.1 <- G.1.tilde
        G.0 <- setdiff(seq(1,p),G.1)
        marginal.hy[k] <- marginal.count[count]
        print(marginal.hy[k])
        # print(marginal.hy[k])
        break}
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
  if (max(marginal.count)>marginal.FNVS[k]) {
    G1.hy[[k]] <- G.1.tilde.store[l,]
    marginal.hy[k] = marginal.count[l]
  } else {
    G1.hy[[k]] <- G1.FNVS[[k]]
    marginal.hy[k] = marginal.FNVS[k]
  }
  print(name.genes.X1[G1.hy[[k]]])
  print(name.genes.X1[G1.hy[[k]]]%in%ham.genes)
  print(sum(name.genes.X1[G1.hy[[k]]]%in%ham.genes))
} # for (k in 1:k0) ends here
choice.hy <- G1.hy[[which.max(marginal.hy)]]
choice.hy.name <- name.genes.X1[choice.hy]
lm.hy <- lm(y1~X1[,choice.hy]-1)
hat.beta.hy <- rep(0,p)
hat.beta.hy[choice.hy] <- coef(lm.hy)
BIC.hy <- BIC(lm.hy) 
EBIC.hy <- BIC.hy + 2*lchoose(p,length(choice.hy))
print(choice.hy.name)
choice.hy.name%in%ham.genes



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
    EBIC.LASSO0[i0] <- BIC(lm(y1~X1[,choice.lasso]-1)) + 2*lchoose(p,len.choice) 
  }
}

choice.LASSO <- which(hat.BETA.LASSO[,which.min(EBIC.LASSO0)]!=0)
NSP.LASSO <- length(choice.LASSO)
lm.LASSO <- lm(y1~X1[,choice.LASSO]-1)
hat.beta.LASSO <- rep0
hat.beta.LASSO[choice.LASSO] <- coef(lm.LASSO)
BIC.LASSO <- BIC(lm.LASSO)
EBIC.LASSO <- BIC.LASSO + 2*lchoose(p,length(choice.LASSO))
name.genes.X1[choice.LASSO]
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
    EBIC.SCAD0[i0] <- BIC(lm(y1~X1[,choice.scad]-1)) + 2*lchoose(p,len.choice) 
  }
}
choice.SCAD <- which(hat.BETA.SCAD[,which.min(EBIC.SCAD0)]!=0)
NSP.SCAD <- length(choice.SCAD)
lm.SCAD <- lm(y1~X1[,choice.SCAD]-1)
hat.beta.SCAD<- rep0
hat.beta.SCAD[choice.SCAD] <- coef(lm.SCAD)
BIC.SCAD <- BIC(lm(y1~X1[,choice.SCAD]-1))
EBIC.SCAD <- BIC.SCAD + 2*lchoose(p,length(choice.SCAD)) 
name.genes.X1[choice.SCAD]
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
    EBIC.MCP0[i0] <- BIC(lm(y1~X1[,choice.mcp]-1)) + 2*lchoose(p,len.choice) 
  }
}
choice.MCP <- which(hat.BETA.MCP[,which.min(EBIC.MCP0)]!=0)
NSP.MCP <- length(choice.MCP)
lm.MCP <- lm(y1~X1[,choice.MCP]-1)
hat.beta.MCP <- rep0
hat.beta.MCP[choice.MCP] <- coef(lm.MCP)
BIC.MCP <- BIC(lm(y1~X1[,choice.MCP]-1))
EBIC.MCP <- BIC.MCP + 2*lchoose(p,length(choice.MCP)) 
name.genes.X1[choice.MCP]
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
    EBIC.ENET0[i0] <- BIC(lm(y1~X1[,choice.enet]-1)) + 2*lchoose(p,len.choice) 
  }
}
choice.ENET <- which(hat.BETA.ENET[,which.min(EBIC.ENET0)]!=0)
NSP.ENET <- length(choice.ENET)
lm.ENET <- lm(y1~X1[,choice.ENET]-1)
hat.beta.ENET <- rep0
hat.beta.ENET[choice.ENET] <- coef(lm.ENET)
BIC.ENET <- BIC(lm(y1~X1[,choice.ENET]-1))
EBIC.ENET <- BIC.ENET + 2*lchoose(p,length(choice.ENET)) 
name.genes.X1[choice.ENET]

EBIC.ALL <- cbind(EBIC.hy,EBIC.SCAD,EBIC.MCP,EBIC.ENET,EBIC.LASSO)
BIC.ALL <- cbind(BIC.hy,BIC.SCAD,BIC.MCP,BIC.ENET,BIC.LASSO)
BIC.star <- data.frame(rbind(BIC.ALL,EBIC.ALL),row.names = c('BIC','EBIC'))
names(BIC.star) = c('hy','SCAD','MCP','ENET','LASSO')
##########
### CV ###
##########
n1 <- ceiling(n*0.7)
n2 <- n - n1
REP = 500
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
  y2 <- y_shuffle[1:n1] # training
  X3 <-X_shuffle[-(1:n1),]  # testing
  y3 <- y_shuffle[-(1:n1)] # testing
  ## hy ###
  beta.hy <- coef(lm(y2~X2[,choice.hy]))
  pred.hy <- cbind(rep(1,n2),X3[,choice.hy])%*%beta.hy
  MSE.hy[i] <- sqrt(sum((y3-pred.hy)^2)/(n2))
  ## SCAD ###
  beta.SCAD <- coef(lm(y2~X2[,choice.SCAD]))
  pred.SCAD <- cbind(rep(1,n2),X3[,choice.SCAD])%*%beta.SCAD
  MSE.SCAD[i] <- sqrt(sum((y3-pred.SCAD)^2)/(n2))
  ## MCP ###
  beta.MCP <- coef(lm(y2~X2[,choice.MCP]))
  pred.MCP <- cbind(rep(1,n2),X3[,choice.MCP])%*%beta.MCP
  MSE.MCP[i] <- sqrt(sum((y3-pred.MCP)^2)/(n2))
  ## ENET ###
  beta.ENET <- coef(lm(y2~X2[,choice.ENET]))
  pred.ENET <- cbind(rep(1,n2),X3[,choice.ENET])%*%beta.ENET
  MSE.ENET[i] <- sqrt(sum((y3-pred.ENET)^2)/(n2))
  ## LASSO ###
  beta.LASSO <- coef(lm(y2~X2[,choice.LASSO]))
  pred.LASSO <- cbind(rep(1,n2),X3[,choice.LASSO])%*%beta.LASSO
  MSE.LASSO[i] <- sqrt(sum((y3-pred.LASSO)^2)/(n2))
}
NSP.list <- list(choice.hy,choice.SCAD,choice.MCP,choice.ENET,choice.LASSO)
NSP <- unlist(lapply(NSP.list,length))
MSE.pred <- colMeans(cbind(MSE.hy,MSE.SCAD,MSE.MCP,MSE.ENET,MSE.LASSO))
result <- data.frame(cbind(NSP,MSE.pred,t(BIC.star)),
                     row.names = c('FNVS-hy','SCAD','MCP','ENET','LASSO'))
names(result) <- c('NSP','MSE_Pred','BIC','EBIC')

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

## ggplot ##
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


union.gamma <- colSums(rbind(gamma.hy,gamma.SCAD,gamma.MCP,gamma.ENET,gamma.LASSO))
union.nonzero <- which(union.gamma!=0)
all.BETA = rbind(hat.beta.hy,hat.beta.SCAD,hat.beta.MCP,hat.beta.ENET,hat.beta.LASSO)
all.beta <- all.BETA[,union.nonzero]
tag.metohd <- c('Global','SCAD','MCP','ENET','LASSO')
Method <- factor(tag.metohd,levels = tag.metohd[5:1])
data.beta<- data.frame(Method,all.beta)
names(data.beta) <- c('Method',name.genes.X1[union.nonzero])
data.heat <- melt(data.beta,id = 'Method',value.name = 'Coefficient')
p.sig = matrix(NA,5,length(union.nonzero))
p.sig[4,6]='X'

heat.plot <- ggplot(data.heat, aes(x = variable, y = Method, fill = Coefficient)) + 
  geom_tile() +
  scale_fill_gradientn(colours = c("cyan", "white", "red"), 
                       values = scales::rescale(c(min(data.heat$Coefficient), 0, 
                                                  max(data.heat$Coefficient)))) + 
  xlab('Genes') + ylab('Methods') + 
  theme(axis.text.x = element_text(face = 'bold',size=5, angle=90)) + 
  theme(axis.text.y = element_text(face = 'bold',size=10)) + 
  theme(axis.title.x.bottom = element_text(face = 'bold',size=15)) + 
  theme(axis.title.y.left = element_text(face = 'bold',size=15)) + 
  geom_hline(yintercept = seq(0.5,7.5,1),colour='grey80') +
  geom_text(aes(label=as.vector(p.sig)), fontface = 'bold', size = 3)
heat.plot
ggsave('Heatmap.pdf', plot = heat.plot,dpi = 300, height = 6, width = 10)


marginal.data <- data.frame(k = 1:k0, Global = marginal.hy)
marginal.melt = melt(data = marginal.data,id.vars = 'k', variable.name = 'Method')
log.marginal <- ggplot(marginal.melt) + 
  geom_line(aes(x = k, y=value, color = Method), size = 0.5) + 
  geom_point(aes(x = k, y=value, color = Method,shape=Method), size = 1) + 
  ylab('Log Marginal') + scale_x_discrete(limits = seq(1,k0,10)) +
  scale_linetype_manual(values=c("solid","dotted"))
log.marginal
ggsave('Log_marginal.pdf',plot = log.marginal,dpi = 300, height = 6, width = 10)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
xtable(lm.hy)
xtable(lm.SCAD)
xtable(lm.MCP)
xtable(lm.ENET)
xtable(lm.LASSO)

summary(lm.hy)
summary(lm.SCAD)
summary(lm.MCP)
summary(lm.ENET)
summary(lm.LASSO)
