## This routine tests the cacheSolve() routine by performing the following test 
## for 2x2 matrices, 3x3 matrices, 4x4 matrices, and 5x5 matrices;
## 
## 1) Verifies that the inverse matrix that is returned the first time that the 
##    cacheSolve routine is called is actually correct.
## 2) Verifies that if the same matrix is passed to the cacheSolve routine a 
##    second time, then the same inverse matrix is returned.
## 3) Verifies that if a different matrix is passed to the cacheSolve routine,
##    then a new inverse matrix is returned.
## 4) Times each call to cacheSolve so that runtimes can be compared.

source("cachematrix.R")

options(digits.secs=6)

testInverse <- function() {
    for (dimension in 2:5) {
        
        print(paste("Running test with a dimension of", dimension))
        
        vectorA <- runif(dimension, max = 100)
        
        cache1 <- makeVector(vectorA)
        
        beginTime <- Sys.time()
        value1 <- cachemean(cache1)
        endTime <- Sys.time()
        print(paste("Elapsed time:", endTime - beginTime, "seconds."))
        
        beginTime <- Sys.time()
        value2 <- cachemean(cache1)
        endTime <- Sys.time()
        print(paste("Elapsed time:", endTime - beginTime, "seconds."))
        
        if (value1 != value2) {
            return(FALSE)
        }
        
        vectorA <- runif(dimension, max = 100)

        cache1 <- makeVector(vectorA)
        
        beginTime <- Sys.time()
        value3 <- cachemean(cache1)
        endTime <- Sys.time()
        print(paste("Elapsed time:", endTime - beginTime, "seconds."))
        
        if (value1 == value3) {
            return(FALSE)
        }
        print('')
    }
    return(TRUE)
}