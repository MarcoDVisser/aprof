library(aprof,lib='~/R/lib')

 # create function to profile
          foo <- function(N){
                  preallocate<-numeric(N)
                  grow<-NULL
                   for(i in 1:N){
                       preallocate[i]<-N/(i+1)
                       grow<-c(grow,N/(i+1))
                      }
          }
     
          #save function to a source file and reload
          dump("foo",file="foo.R")

  source("foo.R")

                                        # Profile the function
Rprof("foo.out",line.profiling=TRUE)
foo(3e4)
Rprof(append=FALSE)

                                        # Create a aprof object
fooaprof<-aprof("foo.R","foo.out")
saveRDS(fooaprof,"./fooaprof.rds")
