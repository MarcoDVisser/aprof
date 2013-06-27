
filestring<-
"
naivefunction=function(T){

pop=data.frame(SP1=1,SP2=1,time=1)

# Naive way of buidling file
for(i in 2:T){
	SP1=pop$SP1[i-1]*rnorm(1)*(1-(pop$SP1[i-1]+
	(rnorm(1)*pop$SP2[i-1])))
	SP2=pop$SP2[i-1]*rnorm(1)*
	(1-(pop$SP2[i-1]+(rnorm(1)*pop$SP1[i-1])))
	pop=rbind(pop,c(SP1,SP2,i))
}
#useless piece of code
for(i in 2:T){
t(pop)
}

return(pop)
}

"
source(textConnection(filestring))

tmpsource<-tempfile()
write(filestring,tmpsource)

N<-1e3
tmpout<-tempfile()
Rprof(file=tmpout,interval = 0.0002,line.profiling =TRUE)

naivefunction(N)

Rprof(append=F)

CallsInt<-readOutput(tmpout)
readLineDensity(CallsInt$calls,CallsInt$interval)

PlotExcDens(tmpsource,tmpout)
