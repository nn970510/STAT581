bootstrapci<-function(vec0,statfuncs=mean,nboot=10000,alpha=0.05)
{
  #extract sample size, mean and standard deviation from the original data
  n0<-length(vec0)
  mean0<-mean(vec0)
  func0<-statfuncs(vec0)
  sd0<-sqrt(var(vec0))
  # create a vector to store the location of the bootstrap studentized deviation vector
  bootvec<-NULL
  bootorivec<-NULL
  bootbiasvec<-NULL
  jacckbiasvec<-NULL
  jacksdvec<-NULL
  #create the bootstrap distribution using a for loop
  for( i in 1:nboot){
    vecb<-sample(vec0,replace=T)
  #create mean and standard deviation to studentize
    funcb<-statfuncs(vecb)
    sdb<-sqrt(var(vecb))
    meanb<-mean(vecb)
    bootorivec<-c(bootorivec,funcb)
    #note since resampling full vector we can use n0 for sample size of vecb
    bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))
    bootbiasvec<-c(bootbiasvec,meanb-mean0)}
  #Calculate lower and upper quantile of the bootstrap distribution
  bootbias<-mean(bootbiasvec)
  #Original bias/SD
  bootoribias<-mean(bootorivec)-func0
  bootorisd<-sd(bootorivec)
  #Original Normal CI
  NonoptLBN<-func0-qnorm(1-alpha/2)*bootorisd
  NonoptUBN<-func0+qnorm(1-alpha/2)*bootorisd
  #Original Percentile CI
  NonoptLB<-quantile(bootorivec,alpha/2)
  NonoptUB<-quantile(bootorivec,1-alpha/2)
  #Original Pivotal CI
  NonoptLBP<-2*func0-NonoptUB
  NonoptUBP<-2*func0-NonoptLB
  #Professor way to calculate mean, I calculated but did not use and show in result
  lq<-quantile(bootvec,alpha/2)
  uq<-quantile(bootvec,1-alpha/2)
  #ADD the other two confidence intervals.
  thhat<-mean(bootvec)
  se<-sqrt(var(bootvec))
  lqn<-(thhat-qnorm(1-alpha/2)*se)
  uqn<-(thhat+qnorm(1-alpha/2)*se)
  lqp<-2*thhat-uq
  uqp<-2*thhat-lq
  #incorporate into the bootstrap confidence interval (what algebra supports this?) and output result
  LB<-mean0-(sd0/sqrt(n0))*uq
  UB<-mean0-(sd0/sqrt(n0))*lq
  LBN<-mean0-(sd0/sqrt(n0))*uqn
  UBN<-mean0-(sd0/sqrt(n0))*lqn 
  LBP<-mean0-(sd0/sqrt(n0))*uqp
  UBP<-mean0-(sd0/sqrt(n0))*lqp 
  #since I have the mean and standard deviation calculate the normal confidence interval here as well
  NLB<-mean0-(sd0/sqrt(n0))*qnorm(1-alpha/2)
  NUB<-mean0+(sd0/sqrt(n0))*qnorm(1-alpha/2)
  list(optimize_bias=bootbias,original_bias=bootoribias,
       optimize_sd=se,original_sd=bootorisd,
       bootstrap.confidence.interval.percentile.CI=c(LB,UB),bootstrap.confidence.interval.nonoptimize.percentile.CI=c(NonoptLB,NonoptUB),
       bootstrap.confidence.interval.Normal.CI=c(LBN,UBN),bootstrap.confidence.interval.nonoptimize.Normal.CI=c(NonoptLBN,NonoptUBN),
       bootstrap.confidence.interval.Pivotal.CI=c(LBP,UBP),bootstrap.confidence.interval.nonoptimize.Pivotal.CI=c(NonoptLBP,NonoptUBP),
       normal.confidence.interval=c(NLB,NUB))}
  #Jackknife function
Jackknife<-function(v1,statfunc=mean,alpha=0.05){
  #extract sample size, mean and standard deviation from the original data
  n1<-length(v1)
  that<-NULL
  jackvec<-NULL
  func0<-statfunc(v1)
  sd0=sqrt(var(v1))
  #create the Jackknife distribution using a for loop
  for(i in 1:n1){
    funca<-statfunc(v1[-i])
    sdb<-sqrt(var(v1[-i]))
    jackvec<-c(jackvec, n1*(func0)-(n1-1)*funca)
    that<-c(that,funca)}
  #Get bias and sd
  jackbias<-mean(jackvec)-func0
  jacksd<-sd(jackvec)
  #Calculate CI
  LBN<-func0-qnorm(1-alpha/2)*(jacksd/sqrt(n1))
  UBN<-func0+qnorm(1-alpha/2)*(jacksd/sqrt(n1))
  Tlq<-func0-qnorm(1-alpha/2)*(sd0/sqrt(n1))
  Tuq<-func0+qnorm(1-alpha/2)*(sd0/sqrt(n1))
  #List result
  list(jackknife.confidence.interval=c(LBN,UBN),normal.confidence.interval=c(Tlq,Tuq),bias=jackbias,sd=jacksd,sd0=sd0)
  }

vec.sample<-rlnorm(100,3)
bootstrapci(vec.sample,10000,0.05,sd)
Jackknife(vec.sample,sd,0.05)


Sim.func<-function(mu.val=3,n=30,nsim=1000,statfunc){
  #create coverage indicator vectors for bootstrap and normal
  cvec.bootper<-NULL
  cvec.bootnor<-NULL
  cvec.bootpiv<-NULL
  cvec.jack<-NULL
  bias.boot<-NULL
  bias.jack<-NULL
  cvec.norm<-NULL
  #calculate real 
  mulnorm<-(exp(mu.val+1/2))
  #run simulation
  for(i in 1:nsim){
    if((i/10)==floor(i/10)){
      print(i)
      #let me know computer hasntdied
      }
      #sample the simulation vector
    vec.sample<-rlnorm(n,mu.val)
    #bootstrap/jackknife it
    boot.list<-bootstrapci(vec.sample,statfunc)
    jack.list<-Jackknife(vec.sample,statfunc)
    boot.confper<-boot.list$bootstrap.confidence.interval.nonoptimize.percentile.CI
    boot.confnor<-boot.list$bootstrap.confidence.interval.nonoptimize.Normal.CI
    boot.confpiv<-boot.list$bootstrap.confidence.interval.nonoptimize.Pivotal.CI
    jack.conf<-jack.list$jackknife.confidence.interval
    #list bias
    bias.boot<-c(bias.boot,boot.list$original_bias)
    bias.jack<-c(bias.jack,jack.list$bias)
    norm.conf<-boot.list$normal.confidence.interval
    #calculate if confidence intervals include mu
    #count up the coverage by the bootstrap/jackknife interval(Using original way did not /sd/sqrt(n0))
    cvec.bootper<-c(cvec.bootper,( boot.confper[1]<mulnorm)*(boot.confper[2]>mulnorm))
    cvec.bootnor<-c(cvec.bootnor,( boot.confnor[1]<mulnorm)*(boot.confnor[2]>mulnorm))
    cvec.bootpiv<-c(cvec.bootpiv,( boot.confpiv[1]<mulnorm)*(boot.confpiv[2]>mulnorm))
    cvec.jack<-c(cvec.jack,( jack.conf[1]<mulnorm)*(jack.conf[2]>mulnorm))
    #count up the coverage by the normal theory interval
    cvec.norm<-c(cvec.norm,( norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))}
  #calculate and output coverage probability estimates and bias
  list(percentile.boot.coverage=(sum(cvec.bootper)/nsim),
       normal.boot.coverage=(sum(cvec.bootnor)/nsim),
       pivotal.boot.coverage=(sum(cvec.bootpiv)/nsim),
       jack.coverage=(sum(cvec.jack)/nsim),
       norm.coverage=(sum(cvec.norm)/ nsim),
       bootbiasminusjackbias=(mean(bias.boot-bias.jack)))
  }
Sim.func(mu.val=3,n=10,nsim=1000,mean)
Sim.func(mu.val=3,n=30,nsim=1000,mean)
Sim.func(mu.val=3,n=100,nsim=1000,mean)
Sim.func(mu.val=3,n=10,nsim=1000,sd)
Sim.func(mu.val=3,n=30,nsim=1000,sd)
Sim.func(mu.val=3,n=100,nsim=1000,sd)
