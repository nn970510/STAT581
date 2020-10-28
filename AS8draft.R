MLEestimator<-function(vec0,funcname){
  n<<-length(vec0)
  meanv<<-mean(vec0)
  snv=sum((vec0-meanv)^2)
  sumv<<-sum(vec0)
  sqrv=sum(vec0^2)
  if (funcname=="Bernoulli"){
    funct<-function(x)(x^sumv)*((1-x)^(n-sumv))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    paste ("p:", pval$maximum)
    hat=ks.test(vec0,"pbinom",1,pval$maximum)$statistic
    res<-list(para=c(pval$maximum),hatks=hat)
  }
  if (funcname=="Geometric"){
    funct<-function(x)(n*log2(x))+(sumv*log2(1-x))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    hat=ks.test(vec0,"pbinom",1,pval$maximum)$statistic
    res<-list(para=c(pval$maximum),hatks=hat)
  }
  if (funcname=="Normal"){
    mu=sumv/n
    paste("mu:",mu)
    samu=sum((vec0-mu)^2)
    funct<-function(x)-(n/2)*(log(2*pi*x^2)) + (-1/(2*x^2)) *samu
    pval=optimize(funct,c(0,1000),tol=0.001,maximum = TRUE)
    hat=ks.test(vec0,"pnorm",mu,pval$maximum)$statistic
    paste ("sigma:", pval$maximum)
    res<-list(para=c(mu,pval$maximum),hatks=hat)
  }
  return (res)
}
Paraboot<-function(funcname,parameter,l,n,hatks){
  correctrate=0
  cat("hat", hatks)
  for (i in 1:n){
    star=0
    hat=0
    if (funcname=="Bernoulli"){
      data=rbinom(l,1,parameter[1])
      parahatstar=MLEestimator(data,"Bernoulli")$para
      star=ks.test(data,"pbinom",1,parahatstar[1])$statistic
    }
    else if (funcname=="Geometric"){
      data=rgeom(l,parameter[1])
      parahatstar=MLEestimator(data,"Geometric")$para
      star=ks.test(data,"pgeom",parahatstar[1])$statistic
    }
    else if (funcname=="Normal"){
      data=rnorm(l,parameter[1],parameter[2])
      parahatstar=MLEestimator(data,"Normal")$para
      star=ks.test(data,"pnorm",parahatstar[1],parahatstar[2])$statistic
    }
    if (star>hatks){
      correctrate=correctrate+1
    }
  }
  cat (correctrate/n)
}
lis<-MLEestimator(rbinom(1000,1,0.6),"Bernoulli")
hat<-lis$hatks
para<-lis$para
Paraboot("Bernoulli",para,1000,1000,hat)
lis<-MLEestimator(rgeom(1000,0.6),"Geometric")
hat<-lis$hatks
para<-lis$para
Paraboot("Geometric",para,1000,1000,hat)
lis<-MLEestimator(rnorm(1000,3,9),"Normal")
hat<-lis$hatks
para<-lis$para
Paraboot("Normal",para,1000,1000,hat)
