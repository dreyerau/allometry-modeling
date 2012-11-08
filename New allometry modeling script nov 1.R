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

## function to create intial population

start.pop<-function()
{
	
}

## function to reproduce


## selection functions (stabilizing, directional, correlational, proportional)