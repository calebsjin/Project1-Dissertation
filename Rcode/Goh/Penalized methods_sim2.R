library(mvtnorm)
library(MASS)
library(glmnet)
library(ncvreg)
#library(ggplot2)
#library(latex2exp)
library(xtable)
if (!require(matrixStats)) install.packages('matrixStats')
library(matrixStats)
rm(list=ls())
REP <- 2000
p<-200
n<-100
K.upper<-ceiling(n^(2/3))
rep0 <- rep(0,p)
seq.p <- c(1:p)
gamma.true <- rep0
Sig2<-1
RHO <- c(0.9)
rho0<-RHO
hat.gamma.SCAD <- matrix(0,p,REP)
hat.gamma.MCP <- matrix(0,p,REP)
hat.gamma.ENET <- matrix(0,p,REP)
hat.gamma.LASSO <- matrix(0,p,REP)

  FDR.LASSO <- rep(NA,REP)
  FOR.LASSO <- rep(NA,REP)
  t2.LASSO  <- rep(NA,REP)
  n.var.LASSO  <- rep(0,REP)
  n.true.LASSO  <- rep(0,REP)
  ham.LASSO  <- rep(0,REP)
  
  FDR.SCAD <- rep(NA,REP)
  FOR.SCAD <- rep(NA,REP)
  t2.SCAD  <- rep(NA,REP)
  n.var.SCAD  <- rep(0,REP)
  n.true.SCAD  <- rep(0,REP)
  ham.SCAD  <- rep(0,REP)
  
  FDR.MCP <- rep(NA,REP)
  FOR.MCP <- rep(NA,REP)
  t2.MCP  <- rep(NA,REP)
  n.var.MCP  <- rep(0,REP)
  n.true.MCP  <- rep(0,REP)
  ham.MCP  <- rep(0,REP)
  
  FDR.ENET <- rep(NA,REP)
  FOR.ENET <- rep(NA,REP)
  t2.ENET  <- rep(NA,REP)
  n.var.ENET  <- rep(0,REP)
  n.true.ENET  <- rep(0,REP)
  ham.ENET  <- rep(0,REP)
  Sig<-rho0^(abs(matrix(1:p,p,p)-t(matrix(1:p,p,p)))) # AR
  for (k in 1:REP) {
    set.seed(1314+k)
    beta.s <- sample(c(-2,-1,1,2), size = 4, prob = rep(0.25,4), replace = TRUE)
    true.var <- sort(sample(seq.p,4))
    beta = rep(0,p)
    beta[true.var] = beta.s
    false.var <- setdiff(seq.p,true.var)
    gamma.true <- rep0
    gamma.true[true.var] <- 1
    X<-rmvnorm(n,rep(0,p),Sig,method="chol") #By Goh
    y<-as.numeric(X%*%beta)+rnorm(n,mean=0,sd=sqrt(Sig2))
    ###################################
    #1. LASSO #########################
    ###################################
    t1 <- Sys.time()
    fit.LASSO<-glmnet(X,y,intercept=FALSE,family="gaussian",alpha=1) #alpha=1 LASSO, alpha=0 Ridge 
    hat.BETA.LASSO <- as.matrix(fit.LASSO$beta)
    n.lambda <- length(fit.LASSO$lambda)
    EBIC.LASSO <- rep(NA,n.lambda)
    for (i0 in 1:n.lambda) {
      choice.lasso=which(hat.BETA.LASSO[,i0]!=0)
      len.choice <- length(choice.lasso)
      if (len.choice>=1 && len.choice<K.upper) {
        EBIC.LASSO[i0] <- BIC(lm(y~X[,choice.lasso]-1)) + 2*lchoose(p,len.choice) 
      }
    }
    choice.LASSO <- as.numeric(which(hat.BETA.LASSO[,which.min(EBIC.LASSO)]!=0))
    t2.LASSO[k] <- round(difftime(Sys.time(),t1,units = 'secs'), 3)
    n.var.LASSO[k] <- length(choice.LASSO)
    G.0.LASSO = setdiff(seq.p,choice.LASSO)
    FOR.LASSO[k] <- length(setdiff(G.0.LASSO,false.var))/(p-n.var.LASSO[k])
    FDR.LASSO[k] <- length(setdiff(choice.LASSO, true.var))/n.var.LASSO[k]
    hat.gamma.LASSO[choice.LASSO,k] <- 1
    ham.LASSO[k] <- sum(abs(hat.gamma.LASSO[,k]- gamma.true))
    n.true.LASSO[k]<-as.numeric(all(choice.LASSO == true.var))*100
    ###################################
    #2. SCAD ##########################
    ###################################
    t1 <- Sys.time()
    fit.SCAD<-ncvreg(X,y,family="gaussian",alpha=1,penalty="SCAD") #10 folds CV #
    hat.BETA.SCAD <- as.matrix(fit.SCAD$beta)[-1,]
    EBIC.SCAD <- rep(NA, n)
    for (i0 in 1:n) {
      choice.scad=which(hat.BETA.SCAD[,i0]!=0)
      len.choice <- length(choice.scad)
      if (len.choice>=1 && len.choice<K.upper) {
        EBIC.SCAD[i0] <- BIC(lm(y~X[,choice.scad]-1)) + 2*lchoose(p,len.choice) 
      }
    }
    choice.SCAD <- as.numeric(which(hat.BETA.SCAD[,which.min(EBIC.SCAD)]!=0))
    t2.SCAD[k] <- round(difftime(Sys.time(),t1,units = 'secs'), 3)
    n.var.SCAD[k] <- length(choice.SCAD)
    G.0.SCAD = setdiff(seq.p,choice.SCAD)
    FOR.SCAD[k] <- length(setdiff(G.0.SCAD,false.var))/(p-n.var.SCAD[k])
    FDR.SCAD[k] <- length(setdiff(choice.SCAD, true.var))/n.var.SCAD[k]
    hat.gamma.SCAD[choice.SCAD,k] <- 1
    ham.SCAD[k] <- sum(abs(hat.gamma.SCAD[,k]- gamma.true))
    n.true.SCAD[k]<-as.numeric(all(choice.SCAD == true.var))*100
    ####################################
    #3. MCP ############################
    ####################################
    t1 <- Sys.time()
    fit.MCP<-ncvreg(X,y,family="gaussian",alpha=1,penalty="MCP")
    hat.BETA.MCP <- as.matrix(fit.MCP$beta)[-1,]
    EBIC.MCP <- rep(NA,n)
    for (i0 in 1:n) {
      choice.mcp=which(hat.BETA.MCP[,i0]!=0)
      len.choice <- length(choice.mcp)
      if (len.choice>=1 && len.choice<K.upper) {
        EBIC.MCP[i0] <- BIC(lm(y~X[,choice.mcp]-1)) + 2*lchoose(p,len.choice) 
      }
    }
    choice.MCP <- as.numeric(which(hat.BETA.MCP[,which.min(EBIC.MCP)]!=0))
    t2.MCP[k] <- round(difftime(Sys.time(),t1,units = 'secs'), 3)
    n.var.MCP[k] <- length(choice.MCP)
    G.0.MCP = setdiff(seq.p,choice.MCP)
    FOR.MCP[k] <- length(setdiff(G.0.MCP,false.var))/(p-n.var.MCP[k])
    FDR.MCP[k] <- length(setdiff(choice.MCP, true.var))/n.var.MCP[k]
    hat.gamma.MCP[choice.MCP,k] <- 1
    ham.MCP[k] <- sum(abs(hat.gamma.MCP[,k]- gamma.true))
    n.true.MCP[k]<-as.numeric(all(choice.MCP == true.var))*100
    ###################################
    #4. ENET #########################
    ###################################
    t1 <- Sys.time()
    fit.ENET<-glmnet(X,y,intercept=FALSE,family="gaussian",alpha=0.5) #alpha=0.5 ENET, alpha=0 Ridge 
    hat.BETA.ENET <- as.matrix(fit.ENET$beta)
    n.lambda <- length(fit.ENET$lambda)
    EBIC.ENET <- rep(NA,n.lambda)
    for (i0 in 1:n.lambda) {
      choice.enet=which(hat.BETA.ENET[,i0]!=0)
      len.choice <- length(choice.enet)
      if (len.choice>=1 && len.choice<K.upper) {
        EBIC.ENET[i0] <- BIC(lm(y~X[,choice.enet]-1)) + 2*lchoose(p,len.choice) 
      }
    }
    choice.ENET <- as.numeric(which(hat.BETA.ENET[,which.min(EBIC.ENET)]!=0))
    t2.ENET[k] <- round(difftime(Sys.time(),t1,units = 'secs'), 3)
    n.var.ENET[k] <- length(choice.ENET)
    G.0.ENET = setdiff(seq.p,choice.ENET)
    FOR.ENET[k] <- length(setdiff(G.0.ENET,false.var))/(p-n.var.ENET[k])
    FDR.ENET[k] <- length(setdiff(choice.ENET, true.var))/n.var.ENET[k]
    hat.gamma.ENET[choice.ENET,k] <- 1
    ham.ENET[k] <- sum(abs(hat.gamma.ENET[,k]- gamma.true))
    print(k)
    n.true.ENET[k]<-as.numeric(all(choice.ENET == true.var))*100
  }


  ## FDR
  result0 <- array(NA, dim = c(REP,6,4))
  result0[,,1] <- cbind(FDR.SCAD,FOR.SCAD,n.true.SCAD,
                        n.var.SCAD,ham.SCAD,t2.SCAD)
  result0[,,2] <- cbind(FDR.MCP,FOR.MCP,n.true.MCP,
                        n.var.MCP,ham.MCP,t2.MCP)
  result0[,,3] <- cbind(FDR.ENET,FOR.ENET,n.true.ENET,
                        n.var.ENET,ham.ENET,t2.ENET)
  result0[,,4] <- cbind(FDR.LASSO,FOR.LASSO,n.true.LASSO,
                        n.var.LASSO,ham.LASSO,t2.LASSO)
  result0.mean <- apply(result0, FUN = colMeans,3)
  result0.sd <- apply(result0, FUN = colSds, 3)
  result1 <- cbind(result0.mean[1,],result0.sd[1,]/sqrt(REP),result0.mean[2,],result0.sd[2,]/sqrt(REP),result0.mean[3,],result0.sd[3,]/sqrt(REP),
                   result0.mean[4,],result0.sd[4,]/sqrt(REP),result0.mean[5,],result0.sd[5,]/sqrt(REP),result0.mean[6,],result0.sd[6,]/sqrt(REP))
   
  result2 <- data.frame(result1, row.names = c('SCAD','MCP','ENET','LASSO'))
  names(result2) <- c('FDR.mean','FDR.se','FOR.mean','FOR.se',
                      'n.true.mean','n.true.se','n.var.mean','n.var.se',
                      'ham.mean','ham.se','time.mean','time.se')
  print(c(p,rho0))
  result.tex <- xtable(result2,digits = 3) 
  print(result.tex)
  #setwd('~/Dropbox/Goh/Project 2/35/FRE')
  #save(list=ls(),file = paste0('FRE-',n,'-',p,'-',rho0,'.RData')) 





