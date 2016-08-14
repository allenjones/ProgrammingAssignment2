## This routine tests the cacheSolve() routine by performing the following test 
## for 2000x2000 matrices, 2001x2001 matrices, 2002x2002 matrices, and 2003x2003 matrices;
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
    for (dimension in 2000:2003) {
        message("***************************************")
        message(paste("Running test with a dimension of", dimension))
        message("***************************************")
        
        # Create a dimension x dimension matrix containing random data.
        matrixA <- matrix(runif(dimension*dimension, max = 100), dimension, dimension)
        
        cache1 <- makeCacheMatrix(matrixA)
        
        # The first time, the inverse will not already be cached.
        beginTime <- Sys.time()
        inverse1 <- cacheSolve(cache1)
        endTime <- Sys.time()
        message("")
        message(paste("Without caching, the inverse matrix was calculated in", endTime - beginTime, "seconds."))
        
        ## Test to make sure that the value that we got really was the inverse.
        message("")
        message("Multiplying the matrix by its calculated inverse to verify that we get the identity matrix.")
        identityMatrix <- round(matrixA %*% inverse1, 6)
        
        if (identical(identityMatrix, diag(dimension))) {
            message("Verified that the inverse matrix was calculated correctly.")
        }
        else {
            message("The calculated value of the inverse matrix is incorrect.")
            return (FALSE)
        }

        # The second time, the inverse should already be cached.
        message("")
        message("The second time, the inverse should already be cached.")
        beginTime <- Sys.time()
        inverse2 <- cacheSolve(cache1)
        endTime <- Sys.time()
        message(paste("With caching, the inverse matrix was calculated in", endTime - beginTime, "seconds."))
        
        if (!identical(inverse1, inverse2)) {
            return(FALSE)
        }
        
        # Create a new matrix to verify that the old cached value is not used.
        matrixA <- matrix(runif(dimension*dimension, max = 100), dimension, dimension)
        cache1 <- makeCacheMatrix(matrixA)
        inverse3 <- cacheSolve(cache1)
        if (identical(inverse1, inverse3)) {
            return(FALSE)
        }
        message('')
    }
    return(TRUE)
}