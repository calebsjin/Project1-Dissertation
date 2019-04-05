# SIMULATION STUDY

library(mvtnorm)
library(SSL)
library(ncvreg)
library(monomvn)
library(EMVS)
library(sparsenet)
library(parcor)





# ********* Keep track of the following quantities ******** #
# MSE.... mean squared error
# FN .... false negatives
# FP .... false positives
# TP .... true positives
# DIM.... dimensionality
# TRUE... found the true model
# TIME... execution time
# HAM ... Hamming distance


# ******** Methods compared ******************************* #

# SSL oracle
# SSL adaptive
# MCP
# SCAD
# LASSO
# Horseshoe
# EMVS
# MCP sparsenet (two-dimensional cross-validation)

# ******** Hyperparameters ******************************* #

p<-1000

n<-100

block<-matrix(0.9,50,50)


Sigma<-diag(20)%x%block

diag(Sigma)<-1

lambda1<-1      # slab penalty for Spike-and-Slab LASSO

lambda0<-seq(lambda1,n,length=100) # slab penalties for Spike-and-Slab LASSO

L<-length(lambda0)

v0<-seq(0.1,5,by=0.1) # spike variance for EMVS

v1<-100          # slab variance for EMVS

burnin<-200 # burnin for the horseshoe

T<-1000     # length of the MCMC chain for the horseshoe

nrep<-100

q<-6

result<-matrix(0,15,8)

for (i in (1:nrep)){

    # Generate data

        X<-(rmvnorm(n,numeric(p),Sigma))
        
        index<-c(1,51,101,151,201,251)  # choose  location of nonzero entries

        beta<-numeric(p)

        beta[index]<-1/sqrt(3)*c(-2.5,-2,-1.5,1.5,2,2.5) # assign nonzero entries

        model.true<-numeric(p)

        model.true[index]<-1

        Y<-X%*%beta+rnorm(n)


        # Oracle Spike-and-Slab LASSO

        time<-system.time(result1<-SSL(X, Y,penalty="separable",lambda=lambda0,lambda1=lambda1,theta=q/p,warn=FALSE))

        model<-result1$model

        beta.est<-result1$beta[,L]

        model.est<-numeric(p)

        model.est[model]<-1

     
        result[1,1]<-result[1,1]+sum((beta.est-beta)^2) # MSE

        result[1,2]<-result[1,2]+sum(index%in%model==FALSE)  # False negative
        
        result[1,3]<-result[1,3]+sum(model%in%index==FALSE)  # False positive 

        result[1,4]<-result[1,4]+sum(model%in%index==TRUE)   # True positives

        result[1,5]<-result[1,5]+length(model)               # Dim

        result[1,6]<-result[1,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[1,7]<-result[1,7]+as.numeric(time[3])          # Time

        result[1,8]<-result[1,8]+sum(abs(model.est-model.true)) # Hamming distance

        # Adaptive Spike-and-Slab LASSO

        time<-system.time(result2<-SSL(X, Y,penalty="adaptive",lambda=lambda0,lambda1=lambda1,warn=FALSE))

        model<-result2$model

        beta.est<-result2$beta[,L]

        model.est<-numeric(p)

        model.est[model]<-1
  
        result[2,1]<-result[2,1]+sum((beta.est-beta)^2) # MSE

        result[2,2]<-result[2,2]+sum(index%in%model==FALSE)  # False negative
        
        result[2,3]<-result[2,3]+sum(model%in%index==FALSE)  # False positive 

        result[2,4]<-result[2,4]+sum(model%in%index==TRUE)   # True positives

        result[2,5]<-result[2,5]+length(model)               # Dim

        result[2,6]<-result[2,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[2,7]<-result[2,7]+as.numeric(time[3])       # Time

        result[2,8]<-result[2,8]+sum(abs(model.est-model.true)) # Hamming distance

        
        # MCP

        time<-system.time(cvfit <- cv.ncvreg(X, Y, family=c("gaussian"),penalty="MCP",warn=FALSE))

        result3 <- cvfit$fit

        beta3<- result3$beta[,cvfit$min]

        beta.est<-beta3[-1]

        model<-(1:p)[beta.est!=0]

        model.est<-numeric(p)

        model.est[model]<-1

        result[3,1]<-result[3,1]+sum((beta.est-beta)^2) # MSE

        result[3,2]<-result[3,2]+sum(index%in%model==FALSE)  # False negative
        
        result[3,3]<-result[3,3]+sum(model%in%index==FALSE)  # False positive 

        result[3,4]<-result[3,4]+sum(model%in%index==TRUE)   # True positives

        result[3,5]<-result[3,5]+length(model)               # Dim

        result[3,6]<-result[3,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[3,7]<-result[3,7]+as.numeric(time[3])       # Time

        result[3,8]<-result[3,8]+sum(abs(model.est-model.true)) # Hamming distance

        # SCAD

        time<-system.time(cvfit <- cv.ncvreg(X, Y, family=c("gaussian"),penalty="SCAD",warn=FALSE))

        result4 <- cvfit$fit

        beta4<- result3$beta[,cvfit$min]

        beta.est<-beta4[-1]

        model<-(1:p)[beta.est!=0]

        model.est<-numeric(p)

        model.est[model]<-1

        result[4,1]<-result[4,1]+sum((beta.est-beta)^2) # MSE

        result[4,2]<-result[4,2]+sum(index%in%model==FALSE)  # False negative
        
        result[4,3]<-result[4,3]+sum(model%in%index==FALSE)  # False positive 

        result[4,4]<-result[4,4]+sum(model%in%index==TRUE)   # True positives

        result[4,5]<-result[4,5]+length(model)               # Dim

        result[4,6]<-result[4,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[4,7]<-result[4,7]+as.numeric(time[3])       # Time

        result[4,8]<-result[4,8]+sum(abs(model.est-model.true)) # Hamming distance

        
        # LASSO

        time<-system.time(cvfit <- cv.ncvreg(X, Y, family=c("gaussian"),penalty="lasso",warn=FALSE))

        result5 <- cvfit$fit

        beta5<- result5$beta[,cvfit$min]

        beta.est<-beta5[-1]

        model<-(1:p)[beta.est!=0]

        model.est<-numeric(p)

        model.est[model]<-1

        result[5,1]<-result[5,1]+sum((beta.est-beta)^2) # MSE
        
        result[5,2]<-result[5,2]+sum(index%in%model==FALSE)  # False negative
        
        result[5,3]<-result[5,3]+sum(model%in%index==FALSE)  # False positive 

        result[5,4]<-result[5,4]+sum(model%in%index==TRUE)   # True positives

        result[5,5]<-result[5,5]+length(model)               # Dim

        result[5,6]<-result[5,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[5,7]<-result[5,7]+as.numeric(time[3])       # Time

        result[5,8]<-result[5,8]+sum(abs(model.est-model.true)) # Hamming distance


        # Horseshoe

        time<-system.time(result6<-bhs(X,Y,T=T))

        s <- summary(result6, burnin=burnin)

        beta.est<-colMeans(result6$beta[-(1:burnin),])

        model<-(1:p)[s$bn0>0.5]

        model.est<-numeric(p)

        model.est[model]<-1


        result[6,1]<-result[6,1]+sum((beta.est-beta)^2) # MSE

        result[6,2]<-result[6,2]+sum(index%in%model==FALSE)  # False negative
        
        result[6,3]<-result[6,3]+sum(model%in%index==FALSE)  # False positive 

        result[6,4]<-result[6,4]+sum(model%in%index==TRUE)   # True positives

        result[6,5]<-result[6,5]+length(model)               # Dim

        result[6,6]<-result[6,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[6,7]<-result[6,7]+as.numeric(time[3])       # Time

        result[6,8]<-result[6,8]+sum(abs(model.est-model.true)) # Hamming distance

        # EMVS

        
        time<-system.time(result8<-EMVS(Y,X,v0=v0,v1=v1,type="betabinomial",beta_init=rep(1,p),sigma_init=1,epsilon=10^{-4},a=1,b=p,direction="backward"))

        model<-EMVSbest(result8)$indices

        beta.est<-result8$betas[which.max(result8$log_g_function),]
        
        model.est<-numeric(p)

        model.est[model]<-1

        result[7,1]<-result[7,1]+sum((beta.est-beta)^2) # MSE

        result[7,2]<-result[7,2]+sum(index%in%model==FALSE)  # False negative
        
        result[7,3]<-result[7,3]+sum(model%in%index==FALSE)  # False positive 

        result[7,4]<-result[7,4]+sum(model%in%index==TRUE)   # True positives

        result[7,5]<-result[7,5]+length(model)               # Dim

        result[7,6]<-result[7,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[7,7]<-result[7,7]+as.numeric(time[3])       # Time

        result[7,8]<-result[7,8]+sum(abs(model.est-model.true)) # Hamming distance

        
        # MCP (sparsenet)

        time<-system.time(fitcv<-cv.sparsenet(X,Y,lambda=result3$lambda))

        beta.est<-coef(fitcv,X,which="parms.1se")[-1]

        model<-(1:p)[beta.est!=0]

        model.est<-numeric(p)

        model.est[model]<-1

        result[8,1]<-result[8,1]+sum((beta.est-beta)^2) # MSE

        result[8,2]<-result[8,2]+sum(index%in%model==FALSE)  # False negative
        
        result[8,3]<-result[8,3]+sum(model%in%index==FALSE)  # False positive 

        result[8,4]<-result[8,4]+sum(model%in%index==TRUE)   # True positives

        result[8,5]<-result[8,5]+length(model)               # Dim

        result[8,6]<-result[8,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[8,7]<-result[8,7]+as.numeric(time[3])       # Time
    
        result[8,8]<-result[8,8]+sum(abs(model.est-model.true)) # Hamming distance

        # Separable Spike-and-Slab LASSO

        time<-system.time(result1<-SSL(X, Y,penalty="separable",lambda=lambda0,lambda1=lambda1,theta=0.5,warn=FALSE))

        model<-result1$model

        beta.est<-result1$beta[,L]
     
        model.est<-numeric(p)

        model.est[model]<-1

        result[9,1]<-result[9,1]+sum((beta.est-beta)^2) # MSE

        result[9,2]<-result[9,2]+sum(index%in%model==FALSE)  # False negative
        
        result[9,3]<-result[9,3]+sum(model%in%index==FALSE)  # False positive 

        result[9,4]<-result[9,4]+sum(model%in%index==TRUE)   # True positives

        result[9,5]<-result[9,5]+length(model)               # Dim

        result[9,6]<-result[9,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[9,7]<-result[9,7]+as.numeric(time[3])       # Time

        result[9,8]<-result[9,8]+sum(abs(model.est-model.true)) # Hamming distance


        # Adaptive Spike-and-Slab LASSO lambda=0.5

        time<-system.time(result2<-SSL(X, Y,penalty="adaptive",lambda=seq(0.5,n,length=100),lambda1=0.5,warn=FALSE))

        model<-result2$model

        beta.est<-result2$beta[,L]

      
        model.est<-numeric(p)

        model.est[model]<-1

        result[10,1]<-result[10,1]+sum((beta.est-beta)^2) # MSE

        result[10,2]<-result[10,2]+sum(index%in%model==FALSE)  # False negative
        
        result[10,3]<-result[10,3]+sum(model%in%index==FALSE)  # False positive 

        result[10,4]<-result[10,4]+sum(model%in%index==TRUE)   # True positives

        result[10,5]<-result[10,5]+length(model)               # Dim

        result[10,6]<-result[10,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[10,7]<-result[10,7]+as.numeric(time[3])       # Time

        result[10,8]<-result[10,8]+sum(abs(model.est-model.true)) # Hamming distance

    # Adaptive Spike-and-Slab LASSO lambda=2

        time<-system.time(result2<-SSL(X, Y,penalty="adaptive",lambda=seq(2,n,length=100),lambda1=2,warn=FALSE))

        model<-result2$model

        beta.est<-result2$beta[,L]

      
        model.est<-numeric(p)

        model.est[model]<-1

        result[11,1]<-result[11,1]+sum((beta.est-beta)^2) # MSE

        result[11,2]<-result[11,2]+sum(index%in%model==FALSE)  # False negative
        
        result[11,3]<-result[11,3]+sum(model%in%index==FALSE)  # False positive 

        result[11,4]<-result[11,4]+sum(model%in%index==TRUE)   # True positives

        result[11,5]<-result[11,5]+length(model)               # Dim

        result[11,6]<-result[11,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[11,7]<-result[11,7]+as.numeric(time[3])       # Time

        result[11,8]<-result[11,8]+sum(abs(model.est-model.true)) # Hamming distance

     # Adaptive Spike-and-Slab LASSO lambda=0.1

        time<-system.time(result2<-SSL(X, Y,penalty="adaptive",lambda=seq(0.1,n,length=100),lambda1=0.1,warn=FALSE))

        model<-result2$model

        beta.est<-result2$beta[,L]

      
        model.est<-numeric(p)

        model.est[model]<-1

        result[12,1]<-result[12,1]+sum((beta.est-beta)^2) # MSE

        result[12,2]<-result[12,2]+sum(index%in%model==FALSE)  # False negative
        
        result[12,3]<-result[12,3]+sum(model%in%index==FALSE)  # False positive 

        result[12,4]<-result[12,4]+sum(model%in%index==TRUE)   # True positives

        result[12,5]<-result[12,5]+length(model)               # Dim

        result[12,6]<-result[12,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[12,7]<-result[12,7]+as.numeric(time[3])       # Time

        result[12,8]<-result[12,8]+sum(abs(model.est-model.true)) # Hamming distance

     # Adaptive Spike-and-Slab LASSO lambda=3

        time<-system.time(result2<-SSL(X, Y,penalty="adaptive",lambda=seq(3,n,length=100),lambda1=3,warn=FALSE))

        model<-result2$model

        beta.est<-result2$beta[,L]

      
        model.est<-numeric(p)

        model.est[model]<-1

        result[13,1]<-result[13,1]+sum((beta.est-beta)^2) # MSE

        result[13,2]<-result[13,2]+sum(index%in%model==FALSE)  # False negative
        
        result[13,3]<-result[13,3]+sum(model%in%index==FALSE)  # False positive 

        result[13,4]<-result[13,4]+sum(model%in%index==TRUE)   # True positives

        result[13,5]<-result[13,5]+length(model)               # Dim

        result[13,6]<-result[13,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[13,7]<-result[13,7]+as.numeric(time[3])       # Time

        result[13,8]<-result[13,8]+sum(abs(model.est-model.true)) # Hamming distance


         # MCP  best subset

     

        time<-system.time(cvfit <- cv.ncvreg(X, Y, family=c("gaussian"),penalty="MCP",warn=FALSE,gamma=1.0001))

        result3 <- cvfit$fit

        beta3<- result3$beta[,cvfit$min]

        beta.est<-beta3[-1]

        model<-(1:p)[beta.est!=0]

        model.est<-numeric(p)

        model.est[model]<-1
        
        result[14,1]<-result[14,1]+sum((beta.est-beta)^2) # MSE

        result[14,2]<-result[14,2]+sum(index%in%model==FALSE)  # False negative
        
        result[14,3]<-result[14,3]+sum(model%in%index==FALSE)  # False positive 

        result[14,4]<-result[14,4]+sum(model%in%index==TRUE)   # True positives

        result[14,5]<-result[14,5]+length(model)               # Dim

        result[14,6]<-result[14,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[14,7]<-result[14,7]+as.numeric(time[3])       # Time

        result[14,8]<-result[14,8]+sum(abs(model.est-model.true)) # Hamming distance


        # Adaptive lasso
        
        
        time<-system.time(result2<-ada.object<-adalasso(X,Y,k=10,intercept=F))

        beta3<- result2$coefficients.adalasso

        beta.est<-beta3

        model<-(1:p)[beta.est!=0]

        model.est<-numeric(p)

        model.est[model]<-1
     
      
        result[15,1]<-result[15,1]+sum((beta.est-beta)^2) # MSE

        result[15,2]<-result[15,2]+sum(index%in%model==FALSE)  # False negative
        
        result[15,3]<-result[15,3]+sum(model%in%index==FALSE)  # False positive 

        result[15,4]<-result[15,4]+sum(model%in%index==TRUE)   # True positives

        result[15,5]<-result[15,5]+length(model)               # Dim

        result[15,6]<-result[15,6]+as.numeric(sum(model%in%index==TRUE)==q & length(model)==q)       # Found true model?

        result[15,7]<-result[15,7]+as.numeric(time[3])       # Time

        result[15,8]<-result[15,8]+sum(abs(model.est-model.true)) # Hamming distance
        
    
    }


result<-data.frame(result)
colnames(result)<-c("MSE","FN","FP","TP","DIM","TRUE","TIME","HAM")
rownames(result)<-c("SSL_oracle","SSL_adaptive","MCP","SCAD","LASSO","Horseshoe","EMVS","MCP(sparsenet)","SSL_sep","SSL_lambda1_0.5","SSL_lambda1_2","SSL_lambda1_0.1","SSL_lambda1_3",
                    "MCP_hard","Adaptive_lasso")


