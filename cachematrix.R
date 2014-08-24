## The functions below calculate the inverse of a matrix. 
## The functions assume that the input matrix is always inversible. 


## makeCacheMatrix: This function creates a list of functions that 
## set the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix. 
## 
## Input: matrix object whose inverse is to be calculated.
## Output: matrix object with all the functions in its context. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: This method gets the cached inverse matrix if already cached
## ( possibly by a previous computation). It skips the calculation if cached 
## value is present. If not, it calulates the inverse matrix using solve ()
##
## Input : matrixobject with the set and get functions context 
## Output: Returns the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
