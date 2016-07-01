## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix function creates a special "matrix" that can ##cache its matrix. 
##get function returns the vector x stored in the main function
##set function changes the vector stored
##getinverse and setinverse are like set and get functions which ##store the value of input in a variable m 

##cacheSolve function uses the solve method to find the inverse ##of the matrix returned by the makeCacheMatrix function. if the ##inverse is already found, then the cacheSolve returns the ##inverse from the cache. If not calculated, inverse of the ##matrix is found, and stored in makeCacheMatrix.

## Write a short comment describing this function

## This function creates a special "matrix" object that can ##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	 m <- NULL
    set <- function(y) {
         x <<- y
         m <<- NULL
    }
    get <- function() x
    setinverse = function(solve) m <<- solve
    getinverse = function() m
    list(set = set, get = get, 
	 setinverse = setinverse, 
         getinverse = getinverse)

}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" ##returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has ##not changed), then the cachesolve should retrieve the inverse ##from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
