library(MASS)

MLEestimator<-function(vec0,funcname){
  n<<-length(vec0)
  meanv<<-mean(vec0)
  snv=sum((vec0-meanv)^2)
  sumv<<-sum(vec0)
  sqrv=sum(vec0^2)
  if (funcname=="Bernoulli"){
    funct<-function(x)(x^sumv)*((1-x)^(n-sumv))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    print(paste ("p:", pval$maximum))
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
  if (funcname=="Binomial"){
  	nval = (1.0/p_true)*mean(vec0)

  	# max log likelihood 
    funct<-function(x) (sumv*log(x) + (length(vec0)*nval-sumv)*log(1-x))
    pval=optimize(funct,c(0,1),tol=0.0001,maximum = TRUE)
    print(paste ("p:", pval$maximum))
  }
  if (funcname=="Exponential"){
  	# max log likelihood 
    funct<-function(x) (length(vec0)*log(x) - x*sum(vec0))
    lambdaval=optimize(funct,c(0,lambda_true+1000),tol=0.0001,maximum = TRUE)
    print(paste ("lambda:", lambdaval$maximum))
  }
  if(funcname=="Beta"){
	loglik <- function(mu, x) { 
		sum(-dbeta(x,mu[1],mu[2],log = TRUE)) 
	}  
	# max log likelihood
	out <- optim(par = c(1,1), fn=loglik,x=vec0,method = "L-BFGS-B",lower=c(0,0))
	print(paste("alpha:", out$par[1]))
	print(paste("beta:", out$par[2]))
  }
  if(funcname=="Multinomial"){
  	n0=sum(vec0[,1])
  	vec1<-apply(vec0, 2, "/", n0)
	out<-rowMeans(vec1)
	print("pvec:")
	print(out)
  }
}


MLEestimator(rbinom(1000,1,0.6),"Bernoulli")
MLEestimator(rgeom(1000,0.6),"Geometric")
MLEestimator(rnorm(1000,3,3),"Normal")
MLEestimator(rchisq(1000,2),"Chisq")
Sigma <- matrix(c(10,3,3,2),2,2)
mvn=mvrnorm(n=1000, rep(0, 2), Sigma)
MLEestimator(mvn,"Multivariate Normal")

n_true<-10000
p_true<-0.76
MLEestimator(rbinom(1000,n_true,p_true),"Binomial")

lambda_true=13
MLEestimator(rexp(1000,rate=lambda_true),"Exponential")

alpha_true=7
beta_true=4
MLEestimator(rbeta(1000,alpha_true,beta_true),"Beta")

n_true<-10000
pvec_true<-c(0.4, 0.21, 0.095, 0.043, 0.067, 0.185)
MLEestimator(rmultinom(1, n_true, pvec_true),"Multinomial")