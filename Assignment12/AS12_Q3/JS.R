JS<-function(){
  mleerr=NULL
  jserr=NULL
  for (k in 1:100){
    X<-matrix(nrow=1000,ncol=k)
    theta<-matrix(nrow=1,ncol=k)
    for (j in 1:k){
      theta[1,j]=runif(1,-5,5)
      newdata=rnorm(1000,theta[1,j],1)
      X[,j]=newdata
    }
    MLEerror=0
    steinerror=0
    for (line in (1:1000)){
      MLEerror=MLEerror+sum((theta-mean(X[line,]))^2)
      steinerror=steinerror+sum((theta-max(1-(k-2)/sum(X[line,]^2),0)*X[line,])^2)
    }
    mleerr=c(mleerr,MLEerror/1000)
    jserr=c(jserr,steinerror/1000)
  }
  list(mle=mleerr,js=jserr)
}
res<-JS()
print (res)
plot(1:100,res$mle, xlab = "k", ylab = "Error", type = "l")
lines(1:100, res$js, col = "red")
legend("topleft", legend = c("JSE", "MLE"), col = c("black", "red"),lwd = 2)
