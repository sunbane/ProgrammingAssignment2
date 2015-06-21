## makeCacheMatrix - function creates a special matrix object that can cache its inverse
## cacheSolve - Function computes the inverse of the special "matrix" returned by makeCacheMatrix

## makeCacheMatrix
##
## 1. set value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get value of the matrix
  get <- function() { x }
  
  # set value of the inverse of non-singular matrix via the solve function
  setInverse <- function(solve) { m <<- solve }
  
  # get value of the inverse
  getInverse <- function() { m }
  
  # pass back
  list (set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
##
## This function returns the inverse of the special "matrix" returned by makeCacheMatrix
## If the matrix has already been calculated, it returns the cached value.  Otherwise it performs
## the calculation, and sets the value in the cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  # check if it is already calculated and if so return the cached matrix
  if (!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
 
  # compute the inverse since it was not already cached
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

# validation 
#
# > mymatrix <- matrix(c(5,2,-6,3,14,1,-8,2,3),3,3)
# > mymatrix
# [,1] [,2] [,3]
# [1,]    5    3   -8
# [2,]    2   14    2
# [3,]   -6    1    3
# > cm <- makeCacheMatrix(mymatrix)
# > cacheSolve(cm)
# [,1]       [,2]        [,3]
# [1,] -0.07380074 0.03136531 -0.21771218
# [2,]  0.03321033 0.06088561  0.04797048
# [3,] -0.15867159 0.04243542 -0.11808118
#
# It works!


