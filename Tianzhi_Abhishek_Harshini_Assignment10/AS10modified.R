SPRT <- function(a, b, theta0, theta1, input_seq) {
	n=length(input_seq)	# stores number of random variables so far
	sum_input_seq = sum(input_seq)

	# compute cumulative sum of log likelihood ratio
	sum_cum_llr = (n * log((1-theta1)/(1-theta0))) + (log((theta1 * (1-theta0)) / (theta0 * (1-theta1))) * sum_input_seq)
	print(paste('cumulative sum of the Log Likelihood Funtions', sum_cum_llr))
	print(paste('<=a', (sum_cum_llr <= a)))
	print(paste('>=b', (sum_cum_llr >= b)))
	print(paste('>a <b', ((sum_cum_llr < b) & (sum_cum_llr > a))))

	res = ifelse((sum_cum_llr>a & sum_cum_llr<b), 0, ifelse(sum_cum_llr<=a, -1, 1))

	return(list(n, input_seq, res))
}

sim <- function(p){
	outcome_seq<-c()
	seq<-c()
	theta0<-0.45
	theta1<-0.55
	alpha<-0.01
	beta<-0.01
	stop<-0

	h0<-'p<=0.45'
	h1<-'p>=0.55'

	a = log(beta / (1-alpha))
	b = log((1-beta) / alpha)
	i=1
	
	while(stop == 0)
	{
		outcome_seq = append(outcome_seq, rbinom(1,1,p))
		seq = append(seq, sum(outcome_seq))
		out = SPRT(a, b, theta0, theta1, outcome_seq)
		print(paste("round:",i,"result",out[3]))
		stop<-out[3]
		i=i+1
	}

	ifelse((out[3] == -1), print(paste("accept: ", h0, "reject: ", h1)), 
		print(paste("accept: ", h1, "reject: ", h0)))
	print('-----------------------------------------------------------')
}

sim(0.3)
sim(0.56)
sim(0.54)
sim(0.5)
sim(0.49)
