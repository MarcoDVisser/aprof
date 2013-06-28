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

readOutput<-function(outputfilename="Rprof.out"){
	#Read and prepare output file
	RprofSamples<-readLines(outputfilename)
	splitCalls<- sapply(RprofSamples[-1],
	function(X) strsplit(X, split = " "),USE.NAMES=F)
	#seperated function calls
	calls<-sapply(splitCalls, function(x) rev(gsub("\"", "", x)))
	#sample.interval (sample rate/micro second)
	Samp.Int<-as.numeric(strsplit(RprofSamples[1],"=")[[1]][2])
	#return function calls and interval
	return(list(calls=calls,interval=Samp.Int*1e-6))
}

readLineDensity<-function(calls,interval,Silent=FALSE){

	cleancalls<-sapply(calls, function(x) 
	gsub("#File", NA, x))

	LineCalls<- sapply(cleancalls,
		function(X) X[grep("#",X)],USE.NAMES=F)
		
	Pathways<-unique(sapply(LineCalls,
		paste,collapse="-"))
	# limit only those containing information
	Pathways<-Pathways[grep("#",Pathways)]

	FirstRow<-sapply(Pathways,function(X) 
	strsplit(X, split = "-")[[1]][1],USE.NAMES=F)

	if(length(unique(FirstRow))==1){
	# remove fist row from calls
	SimpleCalls<-sapply(LineCalls,
		function(X) X[-grep(unique(FirstRow),X)])
	LineDensity<-table(unlist(sapply(SimpleCalls,unique)))    
	} else {
	LineDensity<-table(unlist(sapply(LineCalls,unique)))  
	}
	names(LineDensity)<-gsub("1#","",names(LineDensity))
	Line.Numbers<-as.numeric(names(LineDensity))
	Call.Density<-as.numeric(LineDensity)
	Time.Density<-Call.Density*interval

	Finallist<-list(Line.Numbers=as.numeric(names(LineDensity)),
	Call.Density=as.numeric(LineDensity),
	Time.Density=Call.Density*interval,
	Total.Calls=sum(as.numeric(LineDensity))+1,
	Total.Time=sum(Call.Density*interval+interval))
	
	if(Silent==FALSE){
	# Pretty table 
	CallTable<-cbind(as.character(Finallist$Line.Numbers),
					Finallist$Call.Density,
					Finallist$Time.Density)
	CallTable<-CallTable[order(CallTable[,2]),]
	dimnames(CallTable)<-list(NULL,
					c("Line","Call Density","Time Density (s)"))

					
	  cat("\n Call Density and Execution time per line number:\n\n")
	 print.default(format(CallTable,digits = 3),print.gap = 2L, 
					quote = FALSE)
	  
	  cat("\nInterval (s)\t",interval,"\n\n")
	  
			 cat(paste("\n Totals:\n\n",
			 "Calls\t\t",Finallist$Total.Calls,"\n",
			 "Time (s)\t",Finallist$Total.Time,"\n"))
			
	invisible(Finallist)
	} else{return(Finallist)}
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
		if(length(x)>1){seq(-1,1,length.out=length(x))} 
		else{0}
	)
	
	tmppos<-seq(-1,1,length.out=maxlev)
		
	ypos<-sapply(1:maxlev,function(x) 
		rep(tmppos[x],length(xpos[[x]]))
	)
	

	par(mar=c(0,0,0,0))
	plot(0,0,type='n')

	for(i in 1:maxlev){

		text(xpos[[i]],ypos[[i]],
		names(branches[[i]]),
		cex=branchSize[[i]])

	}
	
}

#pipemodel<-function(calls){

## Number of unique branch pipes
#    pipes<-unique(sapply(calls,
#    paste,collapse=" ",sep=" "))
#    pipesize<-table(sapply(calls,
#    paste,collapse=" ",sep=" "))
	
#	splitPipes<-sapply(pipes,
#	function(X) strsplit(X, split = " "),USE.NAMES=F)
#	NpipeElements<-sapply(splitPipes,length)
	
#	plot(0,0,type="n",ylim=c(1,max(NpipeElements))
	
#	for(i in 1:max(NpipeElements)){
#	rnorm(1)
#	}

	
#}

PlotSourceCode<-function(SourceFilename){

	CodeLines<-readLines(SourceFilename)
	NCodeLines<-length(CodeLines)
	
	CleanLines<-sapply(CodeLines,function(x) 
	gsub("\t", "  ",x,fixed=TRUE),USE.NAMES=F)

	Nchar<-sapply(CleanLines,function(x) 
	strsplit(x,""),USE.NAMES=F)
	Nchar<-sapply(Nchar,function(x) 
	length(x),USE.NAMES=F)
	
	par(mar=c(0,0,0,0))
	plot(0,0,xlim=c(-strwidth("M"),max(Nchar)+strwidth("M")),
	ylim=c(0,NCodeLines),
	type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
	abline(h=1:NCodeLines,col='white')
	#Get best text size
	Codewidth<-sapply(CleanLines,strwidth,USE.NAMES=F)
	Codeheight<-sapply(CleanLines,strheight,USE.NAMES=F)
	
	
	SizeText<-0.98*min(c(
	diff(par("usr")[3:4])/(sum(Codeheight)*1.5),
	diff(par("usr")[1:2])/(max(Codewidth)*1.1))
	)


	ypos<-length(CodeLines):1
	text(1+strwidth("M"),ypos,labels=CleanLines,adj=c(0,0),
	cex=SizeText)
	
	text(0+0.5*strwidth("M"),ypos,labels=1:length(CleanLines),adj=c(1,0),
	cex=SizeText*0.90)
}


PlotExcDens<-function(SourceFilename,outputfilename){

NCodeLines<-length(readLines(SourceFilename))

CallsInt<-readOutput(outputfilename)

LineDensity<-readLineDensity(CallsInt$calls,
			CallsInt$interval,Silent=T)

# Line reversed to correspond to source code plot
DensityData<-list(Lines=NCodeLines:1,
Time.Density=rep(0,NCodeLines))

DensityData$Time.Density[LineDensity$Line.Numbers]<-LineDensity$Time.Density

Spn<-spline(DensityData$Lines,DensityData$Time.Density,n=250)
Spn$y<-ifelse(Spn$y<0,0,Spn$y)


layoutmat<-matrix(c(
			1,1,1,1,3,3,
			rep(c(2,2,2,2,4,4),10)),
			byrow=T,ncol=6)
						
layout(layoutmat)
opar<-par("mar","bg")
par(mar=c(0,0,0,0),bg='grey90')
	plot(0,0,type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
	text(0,0.55,SourceFilename,cex=2)
	segments(-.75,0,.75,0,lwd=1.2)
	segments(c(-.75,.75),c(0,0),c(-.75,.75),c(-0.1,-0.1),lwd=1.2)
PlotSourceCode(SourceFilename)
	plot(0,0,type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
		
	plot(DensityData$Time.Density,DensityData$Lines,
	ylim=c(0,NCodeLines),
	type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
	abline(h=1:NCodeLines,col='white')
	axis(3)
	mtext("Density in execution time(s)",3,cex=1,padj=-3)
	lines(Spn$y,Spn$x,lwd=2,col='grey40')
	polygon(c(Spn$y,0),c(Spn$x,NCodeLines),
	col=rgb(0,0,1,alpha=0.4))
	points(DensityData$Time.Density,DensityData$Lines,
	pch=20)
	par(opar)
}

# Amdahl's law
AmLaw<-function(P=1,S=2){
	1/((1-P)+P/S)
}


# make a pretty Amdahl's profiler table
aprof<-function(calls,interval,type="line"){
if(type=="line"){

LineProf<-readLineDensity(CallsInt$calls,CallsInt$interval,Silent=TRUE)
PropLines<-LineProf$Time.Density/LineProf$Total.Time

Speedups<-2^c(0:4)
SpeedTable<-sapply(Speedups,function(X) AmLaw(P=PropLines,S=X))
dimnames (SpeedTable)<-list(paste("Max Speed-up line", 
LineProf$Line.Numbers,":"),Speedups)
SpeedTable<-SpeedTable[order(PropLines,decreasing=TRUE),]
ExecTimeTable<-LineProf$Total.Time/SpeedTable
ExecTimeTable<-rbind(ExecTimeTable,LineProf$Time.Density/Speedups)

dimnames (SpeedTable)<-list(c(paste("Max improvemnt in time", 
LineProf$Line.Numbers,":"),"All lines"),Speedups)

	  cat("\n Maximum thoeretical attainable speed-up per line number:\n\n")
	  cat("\t\t\t Speed up factor \n")
	 print.default(format(SpeedTable,digits = 3),print.gap = 2L, 
					quote = FALSE)
	  
	 cat("\n Maximum thoeretical attainable improvement in execution time:\n\n")
	 cat("\t\t\t Speed up factor \n")
	 print.default(format(ExecTimeTable,digits = 3),print.gap = 2L, 
					quote = FALSE)
			
	invisible(SpeedTable)

} else {stop("Only line profiling in this version")}


}
