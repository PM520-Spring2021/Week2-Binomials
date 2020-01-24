
set.seed(1473) # sets the random number seed

# This function is the bionomial cumulative distribution function
binom.cdf<-function(x,n,p){
	Fx<-0
	for (i in 0:x){
		Fx <- Fx + choose(n,i)*p^i*(1-p)^(n-i)
	}
	return (Fx)
}

# This function a random variable with an arbitrary CDF (the name of which is 
# passed to it in the first argument)
cdf.sim<-function(F,...){
	X <- 0
	U <- runif(1) # defaults to bounds of 0 and 1
	while (F(X,...)<U){
		X <- X+1
	}
	return (X)
}

MyBinomials<-numeric()
for (i in 1:5000){
	MyBinomials[i]<-cdf.sim(binom.cdf,12,0.5)
}

MyBreaks<-seq(0,13,1)
MyBreaks<-MyBreaks-0.5   
BinHist<-hist(MyBinomials,breaks=MyBreaks)
