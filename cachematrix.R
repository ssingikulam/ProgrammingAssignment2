## Matrix inversion consumes a lot of compute power and there are
## benefits to cache them wherever needed
## The below functions are used to achieve this objective

## makeCacheMatrix:  
## This function takes a matrix as input
## Gets the value of the matrix
## Gets the value of inverse of the matrix
## Sets the inverse of the matrix
## Creates a list of all of these functions

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    setcachedinv <- function(inv) invx <<- inv
    getcachedinv <- function() invx
    
    getinv <- function() x
    list(getcachedinv = getcachedinv,setcachedinv = setcachedinv,getinv = getinv)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse value is already calculated and cached by makeCacheMatrix, cacheSolve
## will retrives that value.  Otherwise, caclculates the inverse and sets the value
## for the cache using setcachedinv function.  Assumption is input is a square matrix

cacheSolve <- function(x, ...) {
      invx <- x$getcachedinv()
      if(!is.null(invx)) {
        message("Returning Cached Data")
        return(invx)
      }
      invdata <- x$getinv()
      invx <- solve(invdata)
      x$setcachedinv(invx)
      invx
      
        ## Return a matrix that is the inverse of 'x'
}

##Sample output
##> source("cachematrix.R")
##> a <- makeCacheMatrix(matrix(1:6,2,2))
## 
##> a$getinv()
##[,1] [,2]
# #[1,]    1    3
# [2,]    2    4
# Get the invrese - No cache
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a)
# Get the inverse and now
# Returning Cached Data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

