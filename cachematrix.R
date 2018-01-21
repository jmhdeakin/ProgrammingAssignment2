## Put comments here that give an overall description of what your
## functions do

## This function creates a cache matrix which takes the provider matrix
## and adds set, get, setCache and getCache methods. Anytime the matrix
## is changed we set the cm 'cachematrix' to null so that it will be
## re-computed in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setCache <- function(cacheMatrix) cm <<- cacheMatrix
  getCache <- function() cm
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## This function takes a cacheMatrix, if the inverse has already been
## computed return it, else re-compute and set the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cm <- x$getCache()
  message("Cached data:")
  print(cm)
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setCache(inverseMatrix)
  inverseMatrix
}


## To run and use the above function you can use the following code commands
## from my cloned/forked repo:
## source("./cachematrix.r")
## myMatrix <- matrix(c(1, 3, 2, 4), nrow = 2, ncol=2)
## cacheMatrix <- makeCacheMatrix(myMatrix)
## cacheSolve(cacheMatrix)
## cacheSolve(cacheMatrix)
## (By running cache solve twice you will see that the cached data is loaded)
