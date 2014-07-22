## There are two functions in this module to calculate the inverse of a matrix

## The first function, makeCacheMatrix creates a special "Matrix", which is really 
##a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                                ## 1. set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                                 ## 2. get the value of the matrix
  setinverse <- function(inverse) m <<- inverse       ## 3. set the value of the inverse
  getinverse <- function() m                          ## 4. get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Using the function makeCacheMatrix(D)
## D must be an invertible square matrix
## returns an object that is a list of functions to work

## example of use
##
## We construct D 
## D=c(1.0, 4, 9, 0.5, 5, 7, 2.0, 6, 11)
## D<-matrix(D,3,3)
##
## We call the function and assigning it to y
## y<-makeMatrix(D)
##

##
## The following function calculates the inverse of the special "matrix" y
## "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                 ## gets the inverse from the cache
  if(!is.null(m)) {                   ## if exist
    message("getting cached data")
    return(m)                         ## returns
  }
  data <- x$get()                     ## If not in cache 
  m <- solve(data)                    ## computes the inverse
  x$setinverse(m)                     ## and saves
  m
}

## example of use
##
## Using the object y created with the function makeMatrix
## computes the inverse
## cacheInverse(y)
##
