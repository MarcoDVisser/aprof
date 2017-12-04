################################################################################
## Test as a result of a bug report by Mark Miller.
## The Monty Hall code was created by Mark Miller
################################################################################

testthat::context('Monty Hall Simulation: memory profiling')
testthat::test_that("Monty Hall Simulation: memory profiling", {
    
    
    foo <- function(N) {
        
        game.interactions  <- 10000
        contestant.action <- rep(NA, game.interactions)
        game.result       <- rep('lose', game.interactions)
        
        for(i in 1:game.interactions) {
            
            door <- c(0,0,0)
            door[sample(3, 1)] = 1            # assign nice prize to a door
                                        # door  with '1' has  nice prize
                                        # doors with '0' have bad  prize
            initial.pick <- sample(3, 1)      # initial contestant action
            not.picked   <- c(1:3)[-initial.pick]
            door.opened.by.host <- not.picked[1]
            if(door[initial.pick   ]==1) door.opened.by.host = not.picked[
                sample(2,1)]
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
    
################################################################################
## NOW PROFILE
################################################################################
## create files to save profiler output and program code
    tmpsrc<-paste0(dirname(tempfile()),"/foo.R")
    tmp<-tempfile()
    
    
    dump("foo",file="foo.R")
    source("foo.R")
    
    Rprof("foo.prof",line.profiling=TRUE,memory.profiling=TRUE)
    foo(1e5)
    Rprof(append=FALSE)
    fooaprof<-aprof::aprof("foo.R","foo.prof")
    sum<-capture.output(aprof:::summary.aprof(fooaprof))


  testthat::expect_that(length(fooaprof$call)>0,testthat::equals(TRUE))
  testthat::expect_that(length(fooaprof$mem$mb)>0,testthat::equals(TRUE))
  testthat::expect_that(length(sum)>0,testthat::equals(TRUE))
  testthat::expect_that(is.character(sum),testthat::equals(TRUE))
  testthat::expect_that(is.numeric(fooaprof$mem$mb),testthat::equals(TRUE))
  testthat::expect_that(fooaprof, testthat::is_a('aprof'))
  testthat::expect_that(fooaprof, testthat::is_a('mem.aprof'))
})

