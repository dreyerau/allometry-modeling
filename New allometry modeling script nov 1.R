### rewriting the functions to model traits and allometry

require(reshape)
require(lattice)
require(dagR)
require(smatr)


# h defines the global value for heritability of sb(0=all environmental, 1=all genetic).  N defines the size of population.

h<-0
N<-1000

## function to generate alleles for each parameter.  right now the function creates 50 "alleles" to be used in generating hypothetical population of traits T1 and T2

alleles<-function(s.x,s.sd,i1.x,i2.x,i1.sd,i2.sd,k1.x,k2.x,k1.sd,k2.sd)
{
	sA<-rnorm(50,(s.x/2),(s.sd/2))
 	sB<-rnorm(50,(s.x/2),(s.sd/2))
 		
 	k1A<-rnorm(50,(k1.x/2),(k1.sd/2))
 	k1B<-rnorm(50,(k1.x/2),(k1.sd/2))
 		
 	i1A<-rnorm(50,(i1.x/2),(i1.sd/2))
 	i1B<-rnorm(50,(i1.x/2),(i1.sd/2))
 		
 	k2A<-rnorm(50,(k2.x/2),(k2.sd/2))
 	k2B<-rnorm(50,(k2.x/2),(k2.sd/2))
 		
 	i2A<-rnorm(50,(i2.x/2),(i2.sd/2))
 	i2B<-rnorm(50,(i2.x/2),(i2.sd/2))

	alle<-data.frame(sA,sB,k1A,k1B,i1A,i1B,k2A,k2B,i2A,i2B)
	
	return(alle)
}

## function to create intial population. 

start.pop<-function(N,s.x,s.sd,i1.x,i2.x,i1.sd,i2.sd,k1.x,k2.x,k1.sd,k2.sd,h)
{
	#use function alleles() to create a set of alleles for popn
	all<-alleles(s.x,s.sd,i1.x,i2.x,i1.sd,i2.sd,k1.x,k2.x,k1.sd,k2.sd)
	
	#generate common systemic factor 's'
	sa<-sample(all$sA,N,replace=T)
 	sb<-sample(all$sB,N,replace=T)
 	#the actual parameter 's' includes an input to control for environmental vs. genetic input, h.  simulating heritability 
 	s<-((sa+sb)*h)+(rnorm(N,s.x,s.sd)*(1-h))
 	
 	#generate T1 specific components
	k1a<-sample(all$k1A,N,replace=T)
 	k1b<-sample(all$k1B,N,replace=T)
 	k1<-(k1a+k1b)
 
 	i1a<-sample(all$i1A,N,replace=T)
 	i1b<-sample(all$i1B,N,replace=T)
 	i1<-(i1a+i1b)

 	b1<-1
 	
 	#create T1
 	T1<-b1*exp(s*k1+i1)
 	
 	#generate T2 specific components
 	k2a<-sample(all$k2A,N,replace=T)
 	k2b<-sample(all$k2B,N,replace=T)
 	k2<-(k2a+k2b)
 
 	i2a<-sample(all$i2A,N,replace=T)
 	i2b<-sample(all$i2B,N,replace=T)
 	i2<-(i2a+i2b)
 	
 	b2<-1
 	
 	#create T2
 	T2<-b2*exp(s*k2+i2)
 	
 	#compile population
 	d<-data.frame(T1,T2,sa,sb,s,i1a,i1b,i1,i2a,i2b,i2,k1a,k1b,k1,k2a,k2b,k2)
 	d$lT1<-log(d$T1)
 	d$lT2<-log(d$T2)
 	
 	return(d)
}

## function to reproduce



## selection functions (stabilizing, directional, correlational, proportional)