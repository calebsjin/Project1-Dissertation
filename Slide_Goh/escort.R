p
P_alpha<-p^10/sum(p^10)
pdf("D:/Dropbox/Shiqiang_share/Project1/manuscript/v9-Goh/escort_fig1.pdf")
a<-1
P_alpha<-p^a/sum(p^a)
names(P_alpha)<-c(1,2,3)
barplot(P_alpha,ylim=c(0,1), ylab="Escort probability")
legend("topright",expression(paste(alpha,"=1")))
dev.off()
a<-0.5
pdf("D:/Dropbox/Shiqiang_share/Project1/manuscript/v9-Goh/escort_fig2.pdf")
P_alpha<-p^a/sum(p^a)
names(P_alpha)<-c(1,2,3)
barplot(P_alpha,ylim=c(0,1), ylab="Escort probability")
legend("topright",expression(paste(alpha,"=0.5")))
dev.off()
a<-0.1
pdf("D:/Dropbox/Shiqiang_share/Project1/manuscript/v9-Goh/escort_fig3.pdf")
P_alpha<-p^a/sum(p^a)
names(P_alpha)<-c(1,2,3)
barplot(P_alpha,ylim=c(0,1), ylab="Escort probability")
legend("topright",expression(paste(alpha,"=0.1")))
dev.off()
