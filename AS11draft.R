wholeest<-function(vec0,funcname,prior){
  n<<-length(vec0)
  meanv<<-mean(vec0)
  snv=sum((vec0-meanv)^2)
  sumv<<-sum(vec0)
  sqrv=sum(vec0^2)  
  if (funcname=="Bernoulli"){
    cat(sprintf("\n ---- Bernoulli ----\n"))
    funct<-function(x)(x^sumv)*((1-x)^(n-sumv))
    phat<-sumv/n
    print(paste ("phat (MOM)=",phat))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    print(paste ("phat (MLE):", pval$maximum))
    p = seq(0,1, length=n)
    hat=ks.test(vec0,"pbinom",1,pval$maximum)$statistic
    print(paste("kstest result:",hat))
    plot(p, dbeta(p, prior[1]+sumv, n-sumv+prior[2]), ylab="density", type ="l")
  } 
  if (funcname=="Geometric"){
    cat(sprintf("\n ---- Geometric ----\n"))
    funct<-function(x)(n*log2(x))+(sumv*log2(1-x))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    print(paste ("phat (MLE):", pval$maximum)) 
    phat=1/((sumv/n)+1)
    print (paste("phat(MOM)=",phat))
    hat=ks.test(vec0,"pgeom",pval$maximum)$statistic
    print(paste("kstest result:",hat))
    p = seq(0,1, length=n)
    plot(p, dbeta(p, prior[1]+n, sumv+prior[2]-1), ylab="density", type ="l")
  }
  if (funcname=="Normal"){
    cat(sprintf("\n ---- Normal ----\n"))
    mu=sumv/n
    print (paste("mu (MLE):",mu))
    samu=sum((vec0-mu)^2)
    funct<-function(x)-(n/2)*(log(2*pi*x^2)) + (-1/(2*x^2)) *samu
    pval=optimize(funct,c(0,1000),tol=0.001,maximum = TRUE)
    print(paste ("sigma (MLE):", pval$maximum))
    mu=sumv/n
    sig2=snv/n
    print(paste("mu(MOM)=",mu,"sigmasquare(MOM)=",sig2))
    hat=ks.test(vec0,"pnorm",mu,pval$maximum)$statistic
    print(paste("kstest result:",hat))
    x = seq(-5,10, length=n)
    odsesq=1/(pval$maximum)^2
    w=odsesq/(odsesq+(1/prior[2]^2))
    plot(x, dnorm(x,w*sumv/n+(1-w)*prior[1],1/(odsesq+(1/prior[2]^2))), ylab="density", type ="l")
  }
}

wholeest(rbinom(1000,1,0.6),"Bernoulli",c(1,1))
wholeest(rgeom(1000,0.6),"Geometric",c(1,1))
wholeest(rnorm(1000,3,5),"Normal",c(5,15))

