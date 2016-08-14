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

testInverse <- function() {
    for (dimension in 2:5) {
        vectorA <- runif(dimension, max = 100)
        print(vectorA)
        cache1 <- makeVector(vectorA)
        print(cache1)
        value1 <- cachemean(cache1)
        print(value1)
        value2 <- cachemean(cache1)
        print(value2)
        if (value1 != value2) {
            return(FALSE)
        }
        vectorA <- runif(dimension, max = 100)
        print(vectorA)
        cache1 <- makeVector(vectorA)
        print(cache1)
        value3 <- cachemean(cache1)
        print(value3)
        if (value1 == value3) {
            return(FALSE)
        }
    }
    return(TRUE)
}