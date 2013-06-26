#  aprof.R
#  
#  Copyright 2013 Marco Visser <marco.d.visser@gmail.com>
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#  
#  

# Read the outfile return by Rprof and prepare for 
# usage in the different programs below.

readOutput<-function(filename="Rprof.out"){
	#Read and prepare output file
	RprofSamples<-readLines(filename)
	splitCalls<- sapply(RprofSamples[-1],
	function(X) strsplit(X, split = " "),USE.NAMES=F)
	#seperated function calls
	calls<-sapply(splitCalls, function(x) rev(gsub("\"", "", x)))
	#sample.interval (sample rate/micro second)
	Samp.Int<-as.numeric(strsplit(RprofSamples[1],"=")[[1]][2])
	#return function calls and interval
	return(list(calls=calls,interval=Samp.Int*1e-6))
}

#Attempt to define brancing structure
MakeBranchPlot<-function(calls,interval){

############### Find stem ################
	nlevel<-sapply(calls,length)
	# shortest branching point
	minlev<-min(nlevel)
	# Tree height
	maxlev<-max(nlevel)
	# Number of unique branch pipes
    pipes<-unique(sapply(calls,
    paste,collapse=" ",sep=" "))
    pipesize<-table(sapply(calls,
    paste,collapse=" ",sep=" "))
	
############### define brances ################
	branches<-vector(maxlev,mode="list")
	for (i in 1:maxlev){
	branches[[i]]<-table(sapply(calls,function(X) X[i]))
	}
	
# Build plot grid represented by a list for each branching
# level

#brance thickness
branTh<-sapply(branches,sum)
branchPropSize<-sapply(branches,function(X) X/max(branTh)*1.5)
branchSize<-sapply(branchPropSize,
function(X) ifelse(X<0.45,0.45,X))


xpos<-sapply(branches, function(x) 
if(length(x)>1){seq(-1,1,length.out=length(x))} else{0}
)
tmppos<-seq(-1,1,length.out=maxlev)
ypos<-sapply(1:maxlev,function(x) rep(tmppos[x],length(xpos[[x]])))

par(mar=c(0,0,0,0))
plot(0,0,type='n')

for(i in 1:maxlev){

text(xpos[[i]],ypos[[i]],
names(branches[[i]]),
cex=branchSize[[i]])

}
	

}

