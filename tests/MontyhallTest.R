setwd("~/symlinks/git/aprof/tests")
################################################################################
## Test as a result of a bug report by Mark Miller.
## The Monty Hall code was created by Mark Miller
################################################################################


foo <- function(N) {

     game.interations  <- 10000
     contestant.action <- rep(NA, game.interations)
     game.result       <- rep('lose', game.interations)

     for(i in 1:game.interations) {

          door <- c(0,0,0)
          door[sample(3, 1)] = 1            # assign nice prize to a door
                                            # door  with '1' has  nice prize
                                            # doors with '0' have bad  prize
          initial.pick <- sample(3, 1)      # initial contestant action
          not.picked   <- c(1:3)[-initial.pick]
          door.opened.by.host <- not.picked[1]
          if(door[initial.pick   ]==1) door.opened.by.host = not.picked[sample(2,1)]
          if(door[  not.picked[1]]==1) door.opened.by.host = not.picked[2]
          contestant.action[i] <- sample(c('k', 's'), 1)
          second.pick <- ifelse(contestant.action[i] == 'k', initial.pick, 
                         not.picked[which(not.picked!=door.opened.by.host)])
          if(door[second.pick]==1) game.result[i] = 'win'
     }

x <- table(contestant.action , game.result)         # examine probability of 
                                                    # winning by action 
prop.table(x)

}

foo(N)

################################################################################
## NOW PROFILE
################################################################################

source("../R/aprof.R")
## save function to a source file and reload
dump("foo",file="foo.R")
source("foo.R")

## create file to save profiler output
tmp<-tempfile()

## Profile the function
Rprof(tmp,line.profiling=TRUE)
foo(1e4)
Rprof(append=FALSE)

## Create a aprof object
fooaprof<-aprof("foo.R",tmp)

## display basic information, summarize and plot the object
fooaprof
summary(fooaprof)
plot(fooaprof)

# another plot
profileplot(fooaprof)

################################################################################
## Now run memory profiler
################################################################################
## To continue with memory profiling:
## enable memory.profiling=TRUE
Rprof(tmp,line.profiling=TRUE,memory.profiling=TRUE)
foo(1e4)
Rprof(append=FALSE)

#
# This line returns the error message below
#
## Create a aprof object
fooaprof<-aprof("foo.R",tmp)
#
# Error in `colnames<-`(`*tmp*`, value = c("sm_v_heap", "lrg_v_heap", "mem_in_node" : 
# 'names' attribute [3] must be the same length as the vector [0]
#

## display basic information, and plot memory usage
fooaprof
plot(fooaprof)
