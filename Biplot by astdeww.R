kota<-scan(what="")
Aceh
Sumut
Sumbar
Riau
Jambi
Sumsel
Bengkulu
Lampung
DKI
Jabar
Jateng
Yogya
Jatim
Kalbar
Kalteng
Kalsel
Kaltim
Sulut
Sulteng
Sulsel
Sultra
Bali
NTB
NTT
Maluku
IRJA
Maluku Utara
Banten

## by Correlation ##
#This syntax is created by Astuti Dewi Warawati

mydata<-read.csv('directory name/mydata.csv', header=T)
Z<-scale(mydata)
tZZ<-t(Z)%*%Z
A<-eigen(tZZ)$vectors
L<-diag(sqrt(eigen(tZZ)$values)) #singular value on X matrices
U<-Z%*%(A%*%solve(L))
t(U)%*%U #proving that U'U=I

#the decomposition -> X=ULA', then X-ULA'=0

G<-U%*%L^0.5 #set the observation coordinate
tH<-L^0.5%*%t(A) #set the variable coordinate
H<-t(tH)

#G<-G*-1 -> can use this too :)
#tH<-tH*-1

dim1<-round(eigen(tZZ)$values[1]/sum(eigen(tZZ)$values)*100,digits=2)
dim2<-round(eigen(tZZ)$values[2]/sum(eigen(tZZ)$values)*100,digits=2)
Rho2<-round((eigen(tZZ)$values[1]+eigen(tZZ)$values[2])/sum(eigen(tZZ)$values)*100,2)

plot(G[,1],G[,2],ylim=c(-2,2),xlim=c(-1,1),'n',
     xlab=paste('Dimension 1:', dim1, " %"),
     ylab=paste('Dimension 2:', dim2, " %"),
     main="Biplot Analisys by astdeww",
     sub=paste('Rho^2= ',round(Rho2,digits=2),' %' ),
      cex.main = 2,   font.main= 4, col.main= "blue",
      cex.sub = 0.75, font.sub = 3, col.sub = "red"
     )
#points(G[,1:2],col='red')
text(G[,1:2],labels=kota,col='red',cex=0.5)
#points(H[,1:2],col='blue')
text(H[,1:2],labels=c("HDI","PDRB","IK","APBD","APSSD",
         	      "APSSLTP","DropSD","DropSMP"),
     col='blue',cex=0.5)
arrows(0,0,H[,1],H[,2],length=0.05,col='blue')
#abline(h=0)
#abline(v=0)

