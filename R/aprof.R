##' Create 'aprof' objects for usage with 'aprof' functions 
##'
##' Creates an "aprof" object from the R-profiler's output and a source file.
##' The objects created through "aprof" can be used by the standard functions
##' plot, summary and print (more specifically:
##' \code{plot.aprof}, \code{summary.aprof} and \code{print.aprof}).
##' See the exampe below for more details.
##' @title Create an 'aprof' objects for usage in the package 'aprof'
##' @param src The name of the source code file (and path if not in the working
##' directory). The source code file is expected to be a 
##' a plain text file (e.g. txt, .R), containing the code of the
##' previously profiled program. If left empty, some "aprof" functions
##' (e.g. \code{readLineDensity}) will attempt to extract this information from
##' the call stack but this is not recommended (as the success of
##' file name detection operations vary). Note that
##' functions that require a defined source file will fail if
##' the source file is not defined or detected in the call stack.
##' 
##' @param output The file name (and path if not in the working
##' directory) of a previously created profiling exercise.
##' @param memoutput  Optional, the file name (and path)
##' of the memory profile from a previous profiling exercise
##' [NOTE: not available in this version]. 
##' @author Marco D. Visser
##' @examples
##' \dontrun{
##'    # create function to profile
##'      foo <- function(N){
##'              preallocate<-numeric(N)
##'              grow<-NULL
##'               for(i in 1:N){
##'                   preallocate[i]<-N/(i+1)
##'                   grow<-c(grow,N/(i+1))
##'                  }
##'      }
##' 
##'      #save function to a source file and reload
##'      dump("foo",file="foo.R")
##'      source("foo.R")
##' 
##'      # create file to save profiler output
##'      tmp<-tempfile()
##' 
##'      # Profile the function
##'      Rprof(tmp,line.profiling=TRUE)
##'      foo(1e4)
##'      Rprof(append=FALSE)
##' 
##'      # Create a aprof object
##'      fooaprof<-aprof("foo.R",tmp)
##'      #display basic information, summarize and plot the object
##'      fooaprof
##'      summary(fooaprof)
##'      plot(fooaprof)
##'      profileplot(fooaprof) 
##'	}
##' @seealso \code{\link{plot.aprof}}, \code{\link{summary.aprof}},
##' \code{\link{print.aprof}}, \code{\link{Rprof}} and
##' \code{\link{summaryRprof}}.
##' @return An aprof object
##' @concept Line profiling
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
# profiler. This is a lower-level function, used to create an "aprof class"
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
        if(length(grep("line profiling",RprofSamples[1]))==0){
        stop("Line profiling is required. \nPlease run the profiler with line profiling enabled")}
	splitCalls<- sapply(RprofSamples[-1],
	function(X) strsplit(X, split = " "),USE.NAMES=FALSE)
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
#' of an aprof object returned by the \code{aprof} function.
#' If a sourcefile was not specified in the aprof object, then the first file
#' within the profiling information is assumed to be the source.
#'
#' @param aprofobject An object returned by \code{aprof}, which
#' contains the stack calls sampled by the R profiler.
#' @param Memprof Logical. Should the function return information
#' specific to memory profiling with memory use per line in Mb?
#' Otherwise, the default is to return line call density and execution time
#' per line [NOTE: This feature is unavailable in the current version].
#' @author Marco D. Visser
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
  ## find all files in the call stack
  idfiles<-sapply(calls,function(X) length(grep("#File", X))>=1)
  ## extract files
  CallFiles <- sapply(calls[idfiles],function(X) X[1])
  
  if(is.null(TargetFile)){
    FileNumber<-"1:"

    warning(paste("sourcefile is null",
                  " assuming first file in call stack is the source: ",
                  CallFiles[1],sep=""))
    
    if(!exists(CallFiles[1])){stop("source file was not defined and does not seem to exist in the working directory.")}
    
  } else{
    unlistedCalls <- unlist(calls)

    ## add path or only stick to basename?
    if(sum(unlistedCalls==TargetFile)==0){
      if(sum(unlistedCalls==basename(TargetFile))>0){
      TargetFile <- basename(TargetFile)
    } else {
       warning(paste("specified source file ", TargetFile,
                  " is not in the list of files in the profiler output: \n ",
                  CallFiles,sep=""))
     }
    } 
    
    FileNumber<-unlistedCalls[which(unlistedCalls==TargetFile)+1]
    FileCheck<-unlistedCalls[which(unlistedCalls==TargetFile)]
    

    ## Confirm that call stack corresponds to user supplied source file
    if(length(FileCheck)==0){
      warning(paste("Some aprof functions may fail -->",
                    " user supplied source file ",TargetFile,
                    " does not seem to correspond to any",
                    " file in the profiler output.\n",
                    " Possible causes: \n" ,
                    "1) Source file was not profiled?\n",
                    "2) Spelling?\n",sep=""))
      
      
    } 
    
  }
  
  FileNumber <- substr(FileNumber,1,1)
  
  ## remove all file references  
  cleancalls<-sapply(calls[!idfiles], function(x) 
                     gsub("#File", NA, x))
  
  LineCalls<- sapply(cleancalls,
                     function(X) X[grep(paste(FileNumber,"#",sep=''),X)],
                     USE.NAMES=FALSE)
  
  Pathways<-unique(sapply(LineCalls, paste,collapse="-"))

  ## limit only those containing information
  Pathways<-Pathways[grep("#",Pathways)]

  filehash <-  paste(FileNumber,"#",sep="")
  LineDensity<-table(unlist(sapply(LineCalls,unique)))  
   names(LineDensity)<-gsub(filehash,"",names(LineDensity))
  Line.Numbers<-as.numeric(names(LineDensity))
  Call.Density<-as.numeric(LineDensity)
  Time.Density<-Call.Density*interval
  
  Finallist<-list(Line.Numbers=as.numeric(names(LineDensity)),
                  Call.Density=as.numeric(LineDensity),
                  Time.Density=Call.Density*interval,
                  Total.Calls=sum(as.numeric(LineDensity))+1,
                  Total.Time=sum(Call.Density*interval+interval),
                  Files=CallFiles)
  
  return(Finallist)
}

#' Generic print method for aprof objects
#'
#' Function that makes a pretty table, and returns
#' some basic information
#' @param x An aprof object returned by the
#' function \code{aprof}.
#' @param \dots Additional printing arguments. Unused.
#' @rdname print
#' @method print aprof 
#' @export
print.aprof <- function(x,...){
aprofobject<-x
 if(!is.aprof(aprofobject)){
   stop("Input does not appear to be of the class \"aprof\"")}

 if(!is.null(aprofobject$sourcefile)){
    cat(paste0("\nSource file:\n",aprofobject$sourcefile," (",
               length(readLines(aprofobject$sourcefile))
               ," lines).\n"))
  }

 
  if(is.null(aprofobject$memcalls)){

    interval <- aprofobject$interval
    Finallist <- readLineDensity(aprofobject,Memprof=FALSE)

	# Pretty table 
    CallTable<-cbind(as.character(Finallist$Line.Numbers),
					Finallist$Call.Density,
					Finallist$Time.Density)

    if(nrow(CallTable)>1) {CallTable<-CallTable[order(CallTable[,2]),]}
    
        rownames(CallTable)<-NULL
	dimnames(CallTable)<-list(NULL, c("Line","Call Density",
                                    "Time Density (s)"))
  
  			
	  cat("\n Call Density and Execution time per line number:\n\n")
	 print.default(format(CallTable,digits = 3),print.gap = 2L, 
					quote = FALSE)
	  
	  		 cat(paste("\n Totals:\n",
			 "Calls\t\t",Finallist$Total.Calls,"\n",
			 "Time (s)\t",Finallist$Total.Time,
                                   "\t(interval = \t",interval,"(s))\n"))
    if(length(Finallist$Files)>1) {
	  cat("\n Note: multiple files in the profiler output: \n")
	 print.default(Finallist$Files,print.gap = 2L,quote = FALSE)
    }
    
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
	dimnames(CallTable)<-list(NULL, c("Line","Call Density",
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
    pipes<-unique(sapply(calls, paste,collapse=" ",sep=" "))
    pipesize<-table(sapply(calls, paste,collapse=" ",sep=" "))
	
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

		text(xpos[[i]],ypos[[i]], names(branches[[i]]),
		cex=branchSize[[i]])

	}
	
}

#pipemodel<-function(calls){

## Number of unique branch pipes
#    pipes<-unique(sapply(calls, #    paste,collapse=" ",sep=" "))
#    pipesize<-table(sapply(calls, #    paste,collapse=" ",sep=" "))
	
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
	gsub("\t", "  ",x,fixed=TRUE),USE.NAMES=FALSE)

	Nchar<-sapply(CleanLines,function(x) 
	strsplit(x,""),USE.NAMES=FALSE)
	Nchar<-sapply(Nchar,function(x) 
	length(x),USE.NAMES=FALSE)
	
	par(mar=c(0,0,0,0))
	plot(0,0,xlim=c(-strwidth("M"),max(Nchar)+strwidth("M")),
	ylim=c(0,NCodeLines+0.5),
	type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
	abline(h=1:NCodeLines,col='white')
	#Get best text size
	Codewidth<-sapply(CleanLines,strwidth,USE.NAMES=FALSE)
	Codeheight<-sapply(CleanLines,strheight,USE.NAMES=FALSE)
	
	
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
#' Plot execution time per line of code from a previously
#' profiled source file. The plot visually shows bottlenecks
#' in a program's execution time, shown directly next to the code
#' of the source file.
#'
#' @param x An aprof object as returned by apof().
#' If this object contains both memory and time profiling information
#' both will be plotted (as proportions of total time and
#' total memory allocations [Note: memory profiling ignored until the next
#' release].
#' @param y Unused and ignored at current.
#' @param \dots Additional printing arguments. Unused at current.
#' 
#' @author Marco D. Visser
#' @examples
#' \dontrun{
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
#' }
#' @concept Line profiling 
#' @rdname plot
#' @method plot aprof 
#' @export
plot.aprof<-function(x,y,...){
aprofobject<-x
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
			1,1,1,1,3,3, rep(c(2,2,2,2,4,4),10)),
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
	points(DensityData$Time.Density,DensityData$Lines, pch=20)
   par(opar)
   layout(1)
}

##' Line progression plot
##' 
##' A profile plot describing the progression through each code
##' line during the execution of the program.
##'
##' Given that a source code file was specified in an "aprof" object
##' this function will estimate when each lines was executed. It
##' identifies the largest bottleneck and indicates this
##' on the plot with red markings (y-axis).
##' R uses a statistical profiler which, using system interrupts,
##' temporarily stops execution of a program at fixed intervals.
##' This is a profiling technique that results in samples of "the call stack"
##' every time the system was stopped. The function \code{profileplot} uses
##' these samples to reconstruct the progression through the
##' program. Note that the best results are obtained when a decent amount of
##' samples have been taken (relative to the length of the source code).
##' Use \code{print.aprof} to see how many samples (termed "Calls") of
##' the call stack were taken.
##' @param aprofobject An aprof object returned by the function
##' \code{aprof}
##' @author Marco D. Visser
##' @examples
##' \dontrun{
##' # create function to profile
##'      foo <- function(N){
##'              preallocate<-numeric(N)
##'              grow<-NULL
##'               for(i in 1:N){
##'                   preallocate[i]<-N/(i+1)
##'                   grow<-c(grow,N/(i+1))
##'                  }
##'      }
##' 
##'      #save function to a source file and reload
##'      dump("foo",file="foo.R")
##'      source("foo.R")
##' 
##'      # create file to save profiler output
##'      tmp<-tempfile()
##' 
##'      # Profile the function
##'      Rprof(tmp,line.profiling=TRUE)
##'      foo(1e4)
##'      Rprof(append=FALSE)
##' 
##'      # Create a aprof object
##'      fooaprof<-aprof("foo.R",tmp)
##'      profileplot(fooaprof)
##' }
##' @seealso \code{\link{plot.aprof}}
##' @concept Line profiling
##' @export

profileplot <- function(aprofobject){
  
 if(!is.aprof(aprofobject)){
   stop("Input does not appear to be of the class \"aprof\"")}

 SourceFilename <- aprofobject$sourcefile

 if(!is.null(aprofobject$memcalls)){
   if(is.null(aprofobject$calls)){stop("profileplot does not work on memory profiles")}}

 if(is.null(SourceFilename)){
    stop("aprof object requires a defined source code file for plotting")
       }
 TargetFile <- aprofobject$sourcefile
 calls<-aprofobject$calls
 interval <- aprofobject$interval
 
 FileNumber<-unlist(calls)[which(unlist(calls)==TargetFile)+1]

 FileNumber <- substr(FileNumber,1,1)

 NCodeLines<-length(readLines(SourceFilename)) 

 cleancalls<-sapply(calls, function(x) 
                    gsub("#File", NA, x))

 LineCalls<- unlist(sapply(cleancalls,
                    function(X) X[grep(paste(FileNumber,"#",sep=''),X)]
                    ,USE.NAMES=FALSE))
 nLineCalls<-as.numeric(sapply(LineCalls,function(X)
                    strsplit(X,"1#")[[1]][2],USE.NAMES=FALSE))
  timesteps<-seq(0,length(nLineCalls)*interval,interval)
  callhistory <- c(1,nLineCalls)
  LineDensity<-readLineDensity(aprofobject)
						
	
 opar<-par("mar","bg")
 maxtimesteps <- max(timesteps)

 layoutmat<-matrix(c(rep(c(1,1,1,1,2,2),10)), byrow=T,ncol=6)
 
 layout(layoutmat)
 
 par(mar=c(4,4,0.1,0.1),bg='grey90')
 plot(0,0,xlim=c(0,maxtimesteps),ylim=c(1,NCodeLines),
      type='n',xaxt='s',yaxt='s', xlab='',ylab='')
 abline(h=1:NCodeLines,col='white')
  
 mtext("Run time(s)",1,cex=.9,padj=3.4)
 mtext("Line",2,cex=.9,padj=-3.4)

 lines(c(timesteps,maxtimesteps), c(callhistory,NCodeLines),
       lwd=2,col=rgb(0,0,1,alpha=0.6))

 text(0,1,"Start",col='red',adj=0,cex=1.2)
 text(maxtimesteps,NCodeLines,"End",col='darkgreen',cex=1.2)
 #largest bottlenecks
 callcounts<-table(callhistory)

 maxcalls<-as.numeric(names(which(callcounts==max(callcounts))))

 axis(2,at=maxcalls,labels=maxcalls,col.axis='red',
      lwd=1.2,col.ticks='red')
 
 plot(0,0,ylim=c(1,NCodeLines),
      xlim = c(0,max(LineDensity$Call.Density/LineDensity$Total.Calls)*1.1),
      type='n',xaxt='s',yaxt='s', xlab='',ylab='')
 
 abline(h = 1:NCodeLines, col = "white")
 PerLineDensity <- numeric(NCodeLines)
 PerLineDensity[LineDensity$Line.Numbers]<-LineDensity$Call.Density/LineDensity$Total.Calls
 connectedlines <- c(1:NCodeLines)-c(0,rep(.5,NCodeLines-2),0)
 lines(y=connectedlines,x=PerLineDensity,type = "S",lwd=1.3)
 abline(v=0,col='grey30',lty=3)
 axis(4)
 mtext("Line Density", 1, cex = .9, padj = 2.7)
 par(opar)
 layout(1) 
 }


#' is.aprof
#'
#' Generic lower-level function to test whether an object
#' is an aprof object.

#' @param object Object to test
#' @export
is.aprof <- function(object) {
  inherits(object, "aprof")
}

# Amdahl's law
#
# function calculates the theoretical maximum
# speed up - at current scaling - of the profiled
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



#' summary.aprof, projections of code optimization gains.
#'
#' Summarizes an "aprof" object and returns a table with
#' the theoretical maximal improvement in execution
#' time for the entire profiled program when a given line
#' of code is sped-up by a factor (called S in the
#' output). Calculations are done using R's profiler
#' output, and requires line profiling to be switched on.
#' Expected improvements are estimated for the entire
#' program using Amdahl's law (Amdahl 1967), and note that
#' Calculations are subject to the scaling of the problem
#' at profiling. The table output aims to answer whether it is
#' worthwhile to spend hours of time optimizing bits of
#' code (e.g. refactoring in C) and, additionally,
#' identifies where these efforts should be focussed.
#' Using aprof one can get estimates of the maximum possible
#' gain. Such considerations are important when one
#' wishes to balance development time vs execution time.
#' All predictions are subject to the scaling of the
#' problem.
#'
#' @param object An object returned by the function \code{aprof}.
#' @param \dots Additional [and unused] arguments.
#' @title Projected optimization gains using Amdahl's law.
#' @references Amdahl, Gene (1967). Validity of the Single Processor
#' Approach to Achieving Large-Scale Computing Capabilities. AFIPS
#' Conference Proceedings (30): 483-485.
#' @author Marco D. Visser
#' @concept Line profiling
#' @rdname summary
#' @method summary aprof 
#' @export
summary.aprof<-function(object,...){

aprofobject<-object

  if(!is.null(aprofobject$memcalls)){
    if(is.null(aprofobject$calls)){
      stop("Projected time gains cannot be summerized for memory profiling")
    }}
   
  if(!is.null(aprofobject$calls)){
    if(!is.null(aprofobject$memcalls)){
    warning("memory profile not used in summary.aprof")}
    
  
	LineProf<-readLineDensity(aprofobject)
	PropLines<-LineProf$Time.Density/LineProf$Total.Time

	Speedups<-2^c(0:4)
	SpeedTable<-sapply(Speedups,function(X) AmLaw(P=PropLines,S=X))

        if(is.null(nrow(SpeedTable))) SpeedTable <- matrix(SpeedTable,nrow=1)
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
  }
  
}


#' targetedSummary
#' 
#' Allows a detailed look into certain lines of code,
#' which have previously been identified as bottlenecks
#' in combination with a source file.
#' 
#' @param target The specific line of code to take a detailed look
#' at. This can be identfied using \code{summary.aprof}.
#' @param aprofobject object of class "aprof" returned by
#' the function \code{aprof}.
#' @param findParent Logical, should an attempt be made to find
#' the parent of a function call? E.g. "lm" would be a parent call of
#' "lm.fit" or "mean" a parent call of "mean.default".
#' Note that currently, the option only returns the most frequently
#' associated parent call when multiple unique parents exist.
#' @param mem Logical, should statistics be adapted to a memory
#' profile? This is only possible if the output from Rprofmem
#' was included in the aprof-object [Note: unavailable for this
#' release].
#' @author Marco D. Visser
#' 
#' @export
targetedSummary<-function(target=NULL,aprofobject=NULL,findParent=FALSE,
                          mem=FALSE){

  if(is.null(target)){stop("Function requires target line number")}

  if(mem) {
    if(is.null(aprofobject$memcalls)){
      stop("mem==TRUE yet no memory profiling detected")
    }
    calls <- aprofobject$memcalls
    interval <- aprofobject$meminterval
  } else {
  if(is.null(aprofobject$calls)){
    stop("calls apear empty (and mem is set to FALSE)")}
    calls <- aprofobject$calls
    interval <- aprofobject$interval
     }
  
  if(is.null(aprofobject$sourcefile)) {
    TargetFile<-"1#"
    warning("sourcefile empty, assumed first file in callstack is the source")
  } else {
    sourcefile <- aprofobject$sourcefile
    FileNumber<-unlist(calls)[which(unlist(calls)==sourcefile)+1]
    TargetFile <- paste(substr(FileNumber,1,1),"#",sep="")
  }

  # identify all unique file names
  FileNames<-unlist(calls)[which(unlist(calls)=="#File")-2]
           
        # What was the total execution time?
  TotalTime<-length(calls)*interval
        # Identify lines of interest
  Lcalls<-sapply(calls,function(x) gsub(TargetFile,"L",x),USE.NAMES=FALSE)
        #Replace all file references with Actual file names

  for(i in 1:length(FileNames)){
    Lcalls<-sapply(Lcalls,function(x) gsub(paste(i,"#",sep='')
                                           ,paste(FileNames[i],
                                                  '#',sep=''),
                                           x),USE.NAMES=FALSE)

         }
        
                                        #Subset to target line
  tlines <- sapply(Lcalls,function(X) paste("L",target,sep='')%in%X)
  TargetCalls<-Lcalls[tlines]

  if(sum(tlines)==0){stop("Target line not found in profiler output.\n
  Confirm target line and run again") }

  
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
          
          ## Sort decending and save as data.frame
        CallOrder <- order(CallCounts,decreasing=TRUE)
        CallCounts <- CallCounts[CallOrder]
        parentCalls <- parentCalls[CallOrder]

        FinalTable<-data.frame(Function=names(CallCounts),
                               Parent=parentCalls, Calls=CallCounts,
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
 
