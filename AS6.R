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
  }
  if (funcname=="Geometric"){
    funct<-function(x)(n*log2(x))+(sumv*log2(1-x))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    paste ("p:", pval$maximum)
  }
  if (funcname=="Normal"){
    mu=sumv/n
    paste("mu:",mu)
    samu=sum((vec0-mu)^2)
    funct<-function(x)-(n/2)*(log(2*pi*x^2)) + (-1/(2*x^2)) *samu
    pval=optimize(funct,c(0,1000),tol=0.001,maximum = TRUE)
    paste ("sigma:", pval$maximum)
  }
  if (funcname=="Chisq"){
    funct<-function(x)(x/2-1)*sum(log2(vec0))-(0.5*sumv)-(n*log2(gamma(x/2)))-((n*x/2)*log2(2))
    pval=optimize(funct,c(1,100),tol=0.001,maximum = TRUE)
    paste ("k:", pval$maximum)
  }
  if (funcname=="Multivariate Normal"){
    mu=apply(vec0,2,mean)
    print(mu)
    sumi=t(vec0-mu)%*%(vec0-mu)
    sumi=sumi/1000
    print (sumi)
  }
}


MLEestimator(rbinom(1000,1,0.6),"Bernoulli")
MLEestimator(rgeom(1000,0.6),"Geometric")
MLEestimator(rnorm(1000,3,3),"Normal")
MLEestimator(rchisq(1000,2),"Chisq")
Sigma <- matrix(c(10,3,3,2),2,2)
mvn=mvrnorm(n=1000, rep(0, 2), Sigma)
MLEestimator(mvn,"Multivariate Normal")
