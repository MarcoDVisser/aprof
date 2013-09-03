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

# Reads the outfile return by Rprof and prepare for 
# usage in the different programs below.

#' readOutput
#' 
#' Reads and organises output files created by the R
#' profiler for further analysis. This program will
#' be updated to generate a "aprof class" for
#' default plotting and printing. 
#'
#' @param outputfilename The file name (and path if not in the working
#' directory) of a previously created profiling exercise.
#' 
#' @author Marco D. Visser
#' 
#' @export

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

#' readLineDensity
#' 
#' Reads and calculates the line density of an aprof object
#' returned by the readOutput function. Returns summary
#' information for the aprof object, when Silent = False.
#'
#' @param calls Stack calls as returned by readOutput
#' @param interval the profiler sampling interval
#' @param TargetFile a plain text file (e.g. txt, .R) including the
#' source code of the previously profiled program.
#' @param Silent Logical. Should the function return summary information?
#' Otherwise the default is to return line call density and execution
#' time counts. Based on the profiler output organized by readOutput.
#' @author Marco D. Visser
#' 
#' @export

readLineDensity<-function(calls,interval,TargetFile=NULL,Silent=FALSE){

  if(is.null(TargetFile)){FileNumber<-"1:"}
  else{FileNumber<-unlist(calls)[which(unlist(calls)==TargetFile)+1]}

  FileNumber <- substr(FileNumber,1,1)
         
	cleancalls<-sapply(calls, function(x) 
	gsub("#File", NA, x))

	LineCalls<- sapply(cleancalls,
		function(X) X[grep(paste(FileNumber,"#",sep=''),X)],USE.NAMES=F)
		
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

# MakeBranchPlot
#
# Incomplete function, originally meant to build
# and plot a tree showing the interdependancy
# between programs in the call stack
#
# @param calls Stack calls as returned by readOutput
# @param interval the profiler sampling interval
# @author Marco D. Visser
# 
#
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

# PlotSourceCode
#
# Helper function, meant to do the actual plotting
# of sourcefile for full program of plotting
# the execution density per line (PlotExDens)
# Eventually these programs will be replace
# through the use of a aprof calls and plot.defaults.
#
# @param SourceFilename  The file name (and path if not in
# the working directory) of source program.
#
# @author Marco D. Visser
# 
#

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

#' PlotExcDens
#'
#' Plot execution density per line of code from a given and previously
#' profiled source file. It attempt to shows you visually where
#' the bottlenecks in execution time are, right in the place where you
#' where are most familiar with the code: your source file.
#'
#' @param SourceFilename The file name (and path if not in
#' the working directory) of source program.
#' @param outputfilename The file name (and path if not in
#' the working directory) of Rprof's output file.
#' 
#' @author Marco D. Visser
#' 
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
#
# function calculates the theoretical maximal
# speed up at current scaling of the profiled
# program using Amdahl's law.
#
# @param P proportion of the program under study
# @param S factor with which P can be sped-up
#
# @author Marco D. Visser
# 
#

AmLaw<-function(P=1,S=2){
	1/((1-P)+P/S)
}


# make a pretty Amdahl's profiler table
#' aprof
#'
#' aprof or "Amdahl's profiler", aims to make directed optimization easy by
#' calculating the theoretical maximal speed up for
#' each line of code (at current scaling) of the profiled
#' program using Amdahl's law. It aims to help anwser question
#' as, whether it is worthwhile to spend hours of time optimizing
#' bits of code (e.i. refactoring in C)?
#' What is going to be your maximum possible gain for the effort
#' you put in? Or is it better to patient and just let the slow program
#' calculate for days on a server somewhere? These considerations are
#' important to know, as idealy one wishes to balance development time
#' vs execution time... or we may end up gaining nothing.
#' Aprof aims to help in this choice.
#' 
#' @title Amdahl's profiler
#' @param calls Stack calls as returned by readOutput
#' @param interval the profiler sampling interval
#' @references Amdahl, Gene (1967). Validity of the Single Processor
#' Approach to Achieving Large-Scale Computing Capabilities. AFIPS
#' Conference Proceedings (30): 483-485.

#' @author Marco D. Visser
#' 
#' @export
aprof<-function(calls,interval,type="line"){

	if(type=="line"){

	LineProf<-readLineDensity(CallsInt$calls,CallsInt$interval,Silent=TRUE)
	PropLines<-LineProf$Time.Density/LineProf$Total.Time

	Speedups<-2^c(0:4)
	SpeedTable<-sapply(Speedups,function(X) AmLaw(P=PropLines,S=X))

	#Time improvement table 
	ExecTimeTable<-LineProf$Total.Time/SpeedTable
	ExecTimeTable<-rbind(ExecTimeTable,LineProf$Total.Time/Speedups)

	# limits of Amdahl's law as S goes to inf
	SpeedTable<-cbind(SpeedTable,1/(1-PropLines))
	dimnames(SpeedTable)<-list(paste("Line*:", 
	LineProf$Line.Numbers,":"),c(Speedups,"S -> Inf**"))
	SpeedTable<-SpeedTable[order(PropLines,decreasing=TRUE),]

	dimnames(ExecTimeTable)<-list(c(paste("Line*:", 
	LineProf$Line.Numbers,":"),"All lines"),Speedups)
	ExecTimeTable<-ExecTimeTable[order(
	c(PropLines,sum(PropLines)),decreasing=TRUE),]

        cat(" Largest attainable speed-up factor for the entire program \n
        when 1 line is sped-up with factor (S): \n\n"

        cat("\t\t\t Speed up factor (S) of a line \n")
        print.default(format(SpeedTable,digits = 3),print.gap = 2L, 
						quote = FALSE)
        cat("Lowest attainable execution time for the entire program when\n             lines are sped-up with factor (S):\n\n" 		  
        cat("\t\t\t Speed up factor (S) of a line  \n")
        print.default(format(ExecTimeTable,digits = 3),print.gap = 2L, 
						quote = FALSE)
        cat("\n    Total sampling time: ",round(LineProf$Total.Time,2) ," seconds")
        cat("\n *  Expected improvement at current scaling")
        cat("\n ** Asymtotic max. improvement at current scaling\n\n")
        
		invisible(SpeedTable)

		} else {stop("Only line profiling in this version")}

}

#' targetedSummary
#' 
#' Allows a detailed look into certain lines of code,
#' which have previously been identified as bottlenecks
#' by PlotExcDens or aprof in combination with a source file.
#' 
#' @param target the specific line of code to take a detailed look
#' at.
#' @param calls Stack calls as returned by readOutput
#' @param interval the profiler sampling interval
#' @param sourcefile a plain text file (e.g. txt, .R) including the
#' source code of the previously profiled program.
#' 
#' @author Marco D. Visser
#' 
#' @export
targetedSummary<-function(target=NULL,calls,interval,sourcefile=NULL){
	
	if(is.null(target)){stop("Function requires target line number")}
        if(is.null(sourcefile)){TargetFile<-"1#"} else {
             FileNumber<-unlist(calls)[which(unlist(calls)==sourcefile)+1]
             TargetFile <- paste(substr(FileNumber,1,1),"#",sep="")
           }

           #identify all unique file names
           FileNames<-unlist(calls)[which(unlist(calls)=="#File")-2]
           
        # What was the total execution time?
        TotalTime<-length(calls)*interval
        #Identify lines of interest
        Lcalls<-sapply(calls,function(x) gsub(TargetFile,"L",x),USE.NAMES=F)
        #Remove all file references with Actual file names
           for(i in 1:length(FileNames)){
           Lcalls<-sapply(Lcalls,function(x) gsub(paste(i,"#",sep='')
                                                 ,paste(FileNames[i],'#',sep=''),
                                                  x),USE.NAMES=F)

         }
        #Subset to target line
        TargetCalls<-Lcalls[sapply(Lcalls,function(X)
                                   paste("L",target,sep='')%in%X)]
        # Remove all functions calls before target line
        trimmedTargetCalls<-lapply(TargetCalls,function(X)
                                   X[1+max(grep(paste("L",target,sep=''),X)):length(X)])
        # Count function calls
        CallCounts<-table(na.omit(unlist(trimmedTargetCalls)))

        # Sort decending and save as data.frame
        CallCounts<-sort(CallCounts,decending=TRUE)

        FinalTable<-data.frame(Function=names(CallCounts),
                               Calls=CallCounts, Time=CallCounts*interval)
        return(FinalTable)
                             

}
