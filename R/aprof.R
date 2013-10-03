##' Amdahl's profiler, directed optimization.
##'
##' Creates an "aprof" object from the R-profiler's output
##' and a source file.
##'
##' .. content for \details{} ..
##' @title Amdahl's profiler
##' @param src The source code file name (and path if not in the working
##' directory) of the program that as been profiled.
##' @param output The file name (and path if not in the working
##' directory) of a previously created profiling exercise.
##' @param memprofile Optional. The file name (and path)
##  of the memory profile of profiling exercise.
##' @author Marco D. Visser
##'
##' @export
aprof <- function(src=NULL,output=NULL,
                  memoutput=NULL){

  if(is.null(src)){
    warning("src is empty, no source code file defined")} else {
      if(!file.exists(src)) {stop(paste("The specified source ",
                                        src, " does not appear to exist"))
                           }
      if(is.na(file.info(src)$size)|file.info(src)$size<1){
        stop("specified source file appears to be empty")}
    }
  
  if(!is.null(output)){
    CallsInt <- readOutput(output)

    if(is.null(CallsInt$calls)|length(CallsInt$calls)==0){
      stop("Rprof outputs appears to be empty, were enough samples made by the profiler?")}
  }


  if(!is.null(memoutput)){ 
    MemCallsInt<-readOutput(memoutput)

    if(is.null(MemCallsInt$calls)|length(MemCallsInt$calls)==0){
    stop("Rprofmem output file seems empty, were enough samples made by the profiler?")
  }


    aprofobject<-list(sourcefile=src,calls=CallsInt$calls,
                      interval=CallsInt$interval, memcalls=MemCallsInt$calls,
                      meminterval=MemCallsInt$interval)

    class(aprofobject) <- c("aprof","list")
    
  } else {

    if(is.null(output)&is.null(memoutput))
       {stop("No profiling output files defined")}
     
    aprofobject<-list(sourcefile=src,calls=CallsInt$calls,
                      interval=CallsInt$interval)

    class(aprofobject) <- c("aprof","list")
  }

  return(aprofobject)
}

# readOutput
# 
# Reads and organises output files created by the R
# profiler, used to create an "aprof class"
# in the function \code{aprof}. 
#
# @param outputfilename The file name (and path if not in the working
# directory) of a previously created profiling exercise.
# 
# @author Marco D. Visser
# 
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
#' Reads and calculates the line density (in execution time or memory)
#' of an aprof object returned by the \code{aprof} function. The function
#' is used internally by many "aprof" functions.
#'
#' @param aprofobject an object returned by \code{aprof}, which
#' contains the stack calls sampled by the R profiler.
#' @param Memprof Logical. Should the function return information
#' specific to memory profiling? As memory use per line and Mb counts?
#' Otherwise the default is to return line call density and execution time
#' counts. 
#' @author Marco D. Visser
#' 
#' @export

readLineDensity<-function(aprofobject=NULL,Memprof=FALSE){

  if(!"aprof"%in%class(aprofobject)){
      stop("no aprof object found, check function inputs")}
  
  
  if(Memprof==TRUE){

    calls <- aprofobject$memcalls
    interval <- aprofobject$meminterval
    TargetFile <- aprofobject$sourcefile
                  }  else {
    calls <- aprofobject$calls
    interval <- aprofobject$interval
    TargetFile <- aprofobject$sourcefile
    }
  
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
	
return(Finallist)
}

#' Generic print method for aprof objects
#'
#' Function makes a pretty table, and returns
#' some basic information
#' @param aprofobject An aprof object return by the
#' function \code{aprof}
#' @export
print.aprof <- function(aprofobject){

 if(!is.aprof(aprofobject)){
   stop("Input does not appear to be of the class \"aprof\"")}
  
  if(is.null(aprofobject$memcalls)){

    interval <- aprofobject$interval
    Finallist <- readLineDensity(aprofobject,Memprof=FALSE)

	# Pretty table 
	CallTable<-cbind(as.character(Finallist$Line.Numbers),
					Finallist$Call.Density,
					Finallist$Time.Density)
	CallTable<-CallTable[order(CallTable[,2]),]
        rownames(CallTable)<-NULL
	dimnames(CallTable)<-list(NULL,
                                  c("Line","Call Density",
                                    "Time Density (s)"))
  
  if(!is.null(aprofobject$src)){
    cat(paste0("\nSource file:\n",aprofobject$src,"/n"))
    cat(paste0("Source length: ", length(readLines(a))," lines"))

  }
					
	  cat("\n Call Density and Execution time per line number:\n\n")
	 print.default(format(CallTable,digits = 3),print.gap = 2L, 
					quote = FALSE)
	  
	  		 cat(paste("\n Totals:\n",
			 "Calls\t\t",Finallist$Total.Calls,"\n",
			 "Time (s)\t",Finallist$Total.Time,
                                   "\t(interval = \t",interval,"(s))\n"))
	  
    
	} else {
         memFinallist <- readLineDensity(aprofobject,Memprof=TRUE)
         meminterval <- aprofobject$meminterval
          if(!is.null(aprofobject$calls)){
            # memory and time profiling

                                        
            
        Finallist <- readLineDensity(aprofobject,Memprof=FALSE)

	# Pretty table 
	CallTable<-cbind(as.character(Finallist$Line.Numbers),
					Finallist$Call.Density,
					Finallist$Time.Density)
	CallTable<-CallTable[order(CallTable[,2]),]
	dimnames(CallTable)<-list(NULL,
                                  c("Line","Call Density",
                                    "Time Density (s)"))

					
	  cat("\n Call Density and Execution time per line number:\n\n")
	 print.default(format(CallTable,digits = 3),print.gap = 2L, 
					quote = FALSE)
	  
	  cat("\nInterval (s)\t",interval,"\n\n")
	  
			 cat(paste("\n Totals:\n\n",
			 "Calls\t\t",Finallist$Total.Calls,"\n",
			 "Time (s)\t",Finallist$Total.Time,"\n"))
			

          } else{
            # only memory

          }
       }
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
	ylim=c(0,NCodeLines+0.5),
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

#' plot.aprof
#'
#' Plot execution density per line of code from a previously
#' profiled source file. The plot visually shows bottlenecks
#' in program execution time, directly where most programmers
#' are most familiar with their code: the source file.
#'
#' @param aprofobject An aprof object as returned by apof().
#' If this object contains both memory and time profiling information
#' both will be plotted (as proportions of total time and
#' total memory allocations.
#' @param zoom Zoom into a particular section of code.
#' Can either be set to "TRUE" or line numbers can be given.
#' If line numbers are given as c(min,max), then the
#' function will attempt to zoom in between these,
#' otherwise "TRUE" will result in a zoom centred around the
#' lines with the greatest density in execution time (and/or
#' memory).
#' 
#' @author Marco D. Visser
#' @examples
#' # create function to profile
#' foo <- function(N){
#'         preallocate<-numeric(N)
#'         grow<-NULL  
#'          for(i in 1:N){
#'              preallocate[i]<-N/(i+1)
#'              grow<-c(grow,N/(i+1))
#'             }
#' }
#'
#' #save function to a source file and reload
#' dump("foo",file="foo.R")
#' source("foo.R")
#'
#' # create file to save profiler output
#' tmp<-tempfile()
#'
#' # Profile the function
#' Rprof(tmp,line.profiling=TRUE)
#' foo(1e4)
#' Rprof(append=FALSE)
#'
#' # Create a aprof object
#' fooaprof<-aprof("foo.R",tmp)
#' plot(fooaprof)
#' @export
plot.aprof<-function(aprofobject,zoom=NULL){

   if(!is.aprof(aprofobject)){
   stop("Input does not appear to be of the class \"aprof\"")}
 
  AddMemProf<-!is.null(aprofobject$memcalls)

  SourceFilename <- aprofobject$sourcefile
  if(is.null(SourceFilename)){
    stop("aprof object requires a defined source code file for plotting")}


  NCodeLines<-length(readLines(SourceFilename))

  LineDensity<-readLineDensity(aprofobject)

# Line reversed to correspond to source code plot
	DensityData<-list(Lines=NCodeLines:1,
	Time.Density=rep(0,NCodeLines))

	DensityData$Time.Density[LineDensity$Line.Numbers]<-LineDensity$Time.Density

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
	ylim=c(0,NCodeLines+0.5),
	type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
	abline(h=1:NCodeLines,col='white')
	axis(3)
	mtext("Density in execution time(s)",3,cex=1,padj=-3)
        segments(0,DensityData$Lines,
                 DensityData$Time.Density,DensityData$Lines
                 ,lwd=4,col=rgb(0,0,1,alpha=0.6))
	points(DensityData$Time.Density,DensityData$Lines,
	pch=20)
	par(opar)
}

#' is.aprof
#'
#' Generic function to test whether an object
#' is an aprof object
#' @param object Object to test
#' @export
is.aprof <- function(object) {
  inherits(object, "aprof")
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



#' summary.aprof
#'
#' Summarizes an "aprof" object and returns a table with
#' the theoretical maximal improvent in execution
#' time for the entire profiled program when a given line
#' of code is sped-up by a factor (called S in the
#' output). Calculations are done using using R's profiler
#' output, and requires line profiling to be switched on.
#' Expected improvements are estimated for the entire
#' program using Amdahl's law (Amdahl 1967). Calculations
#' are subject to the scaling of the problem at profiling.
#' The table output aims to anwser whether it is
#' worthwhile to spend hours of time optimizing bits of
#' code (e.i. refactoring in C) and where these efforts
#' should be focussed. Using aprof one can get estimates
#' of the maximum possible gain for any optimization
#' efforts. Such considerations are important when one
#' wishes to balance development time vs execution time.
#'  
# @title Amdahl's profiler
#' @param calls Stack calls as returned by readOutput.
#' Line profiling must be activated for this to work.
#' @param interval the profiler sampling interval.
#' @param type currently ignored. Only line profiling is
#' available in this version.
#' @references Amdahl, Gene (1967). Validity of the Single Processor
#' Approach to Achieving Large-Scale Computing Capabilities. AFIPS
#' Conference Proceedings (30): 483-485.

#' @author Marco D. Visser
#' 
#' @export
summary.aprof<-function(aprofobject,type="line"){

  if(type=="line"){

	LineProf<-readLineDensity(aprofobject,Silent=TRUE)
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

        cat("Largest attainable speed-up factor for the entire program\n
        when 1 line is sped-up with factor (S): \n\n")

        cat("\t Speed up factor (S) of a line \n")
        print.default(format(SpeedTable,digits = 3),print.gap = 2L, 
						quote = FALSE)
        cat("\nLowest attainable execution time for the entire program when\n
             lines are sped-up with factor (S):\n\n")
        
        cat("\t Speed up factor (S) of a line  \n")
        print.default(format(ExecTimeTable,digits = 3),print.gap = 2L, 
						quote = FALSE)
        cat("\n    Total sampling time: ",round(LineProf$Total.Time,2) ,
            " seconds")
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
#' source code of the previously profiled program. If empty
#' the function will attempt to extract this information from
#' the stack calls.
#' @param findParent Logical, should an attempt be made to find
#' the parent of a function call? E.g. lm would be a parent call of
#' lm.fit or mean a parent call of mean.default. Note that the
#' option only returns the most frequently accociated parent call
#' when multiple unique parents exist.
#' 
#' @author Marco D. Visser
#' 
#' @export
targetedSummary<-function(target=NULL,calls,interval=0.02,sourcefile=NULL,
                          findParent=FALSE){
	
  if(is.null(target)){stop("Function requires target line number")}
  if(is.null(calls)|length(calls)==0){
    stop("calls appear empty, were enough samples made by the profiler?")}
  if(is.null(sourcefile)){TargetFile<-"1#"} else {

    FileNumber<-unlist(calls)[which(unlist(calls)==sourcefile)+1]
             TargetFile <- paste(substr(FileNumber,1,1),"#",sep="")
  }

  # identify all unique file names
  FileNames<-unlist(calls)[which(unlist(calls)=="#File")-2]
           
        # What was the total execution time?
  TotalTime<-length(calls)*interval
        # Identify lines of interest
  Lcalls<-sapply(calls,function(x) gsub(TargetFile,"L",x),USE.NAMES=F)
        #Replace all file references with Actual file names

  for(i in 1:length(FileNames)){
    Lcalls<-sapply(Lcalls,function(x) gsub(paste(i,"#",sep='')
                                           ,paste(FileNames[i],
                                                  '#',sep=''),
                                           x),USE.NAMES=F)

         }
        
        #Subset to target line
        TargetCalls<-Lcalls[sapply(Lcalls,function(X)
                                   paste("L",target,sep='')%in%X)]

        # Remove all functions calls before target line
        trimmedTargetCalls<-lapply(TargetCalls,function(X)
                                   X[1+max(grep(paste("L",target,sep=''),
                                                X)):length(X)])

        # Count function calls
        CallCounts<-table(na.omit(unlist(trimmedTargetCalls)))

        # Find parent call before target call?
        if(findParent==TRUE) {
        # Find unique parent calls for each unique call
          parentCalls <- vector(mode="character", length=
                                length(CallCounts))
          
       for(i in 1:length(CallCounts)){
            parentCalls[i]<-names(sort(table(unlist(
              lapply(TargetCalls,function(X)
                     X[which(names(CallCounts)[i]==X)[1]-1]
                     ))),decreasing=TRUE)[1])
                     
                    
          }
          
        # Sort decending and save as data.frame
        CallOrder <- order(CallCounts,decreasing=TRUE)
        CallCounts <- CallCounts[CallOrder]
        parentCalls <- parentCalls[CallOrder]

        FinalTable<-data.frame(Function=names(CallCounts),
                               Parent=parentCalls,
                               Calls=CallCounts,
                               Time=CallCounts*interval)
        } else {

          CallCounts <- sort(CallCounts,decreasing=TRUE)
          
          FinalTable <- data.frame(Function=names(CallCounts),
                                 Calls=CallCounts,
                                 Time=CallCounts*interval)
        }
        
        row.names(FinalTable) <- NULL
        return(FinalTable)
                             

}
