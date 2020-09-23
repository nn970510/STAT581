Hurr4<-read.csv('HurricanesData.csv')
Hurr5<-read.csv('HurricanesData5.csv')
Hurr4mat<-Hurr4[[2]]
Hurr5mat<-Hurr5[[2]]
count4=as.data.frame(table(Hurr4mat))
count5=as.data.frame(table(Hurr5mat))
len4=length(Hurr4mat)
len5=length(Hurr5mat)
Hurr4lambda<-sum(Hurr4mat)/len4
Hurr5lambda<-sum(Hurr5mat)/len5
chisq4=0
chisq5=0
cind=count4[[1]]
cnum=count4[[2]]
true4=0:max(Hurr4mat)
ind=1
actv=0
expv=0
group=0
for (i in 0:max(Hurr4mat)+1){
  expv=expv+len4*dpois(i-1,Hurr4lambda)
  if (i-1==cind[ind]){
    actv=actv+cnum[ind]
    true4[i]=cnum[ind]
    ind=ind+1
  }
  else{
    true4[i]=0
  }
  if (expv>=5){
    chisq4=chisq4+((expv-actv)^2/expv)
    expv=0
    actv=0
    group=group+1
    next
  }
}


if (expv!=0){
  chisq4=chisq4+((expv-actv)^2/expv)
  group=group+1
}
print ("P_value for hurricane 4")
pchisq(chisq4,group-2)

dist4=dpois(0:max(Hurr4mat),Hurr4lambda)
dist5=dpois(0:max(Hurr5mat),Hurr5lambda)
dist4=dist4*len4
dist5=dist5*len5
qqplot(dist4,true4,xlab = 'Theoretical Quantiles', ylab = 'Empirical Quantiles',
       main = 'Q-Q plot Poisson', xlim = c(0,5), ylim = c(0,5))
abline(0,1)





true5=0:max(Hurr5mat)
cind=count5[[1]]
cnum=count5[[2]]
ind=1
actv=0
expv=0
group=0
for (i in 0:max(Hurr5mat)+1){
  expv=expv+len5*dpois(i-1,Hurr5lambda)
  if (i-1==cind[ind]){
    actv=actv+cnum[ind]
    true5[i]=cnum[ind]
    ind=ind+1
  }
  else {
    true5[i]=0
  }
  if (expv>=5){
    chisq5=chisq5+((expv-actv)^2/expv)
    expv=0
    actv=0
    group=group+1
    next
  }
}
if (expv!=0){
  chisq5=chisq5+((expv-actv)^2/expv)
  group=group+1
}
print("P_Value for Hurricane5")
pchisq(chisq5,group-2)

qqplot(dist5,true5,xlab = 'Theoretical Quantiles', ylab = 'Empirical Quantiles',
       main = 'Q-Q plot Poisson', xlim = c(0,15), ylim = c(0,15))
abline(0,1)

