################################################################################
## Test as a result of a bug report by Mark Miller.
## The Monty Hall code was created by Mark Miller
################################################################################

testthat::context('Monty Hall Simulation: memory profiling')
testthat::test_that("Monty Hall Simulation: memory profiling", {
  
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

