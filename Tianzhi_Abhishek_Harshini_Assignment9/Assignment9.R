FDR<-function(vec,q=0.05,ind=1){
  n=length(vec0)
  qline=c(1:n)*q/n
  argsort=order(vec)
  vec=sort(vec)
  rtn=vector()
  bkpoint=0
  if (ind==0){
    qline=c(1:n)*q/(n*sum(1/c(1:n)))
  }
  for (i in n:1){
    if (qline[i]>vec[i]){
      bkpoint=i
      break
    }
  }
  if (bkpoint!=0){
    for (i in 1:bkpoint){
      rtn=c(rtn,argsort[i])
    }
  }
  cat(rtn,"\n")
  cat("FDR=",length(rtn)/n)
  plot(c(1:n),vec,pch=16,cex=0.5,xlab="data rank",ylab="p value")
  points(c(1:n),qline,pch=16,cex=0.5,col="red")
  legend("topright",lty=c(0,0),pch=16,col=c("black","red"),legend=c("sorted p","qline"))
}
FDR(c(0.000001*runif(100),runif(900)),0.05,0)
