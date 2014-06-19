## Put comments here that give an overall description of what your
## functions do
# The first function creates a 'special' object that is a matrix that is to be 
# inverted and cached by the second function.  The first function itself contains four
# functions, one each to perform:
#  1.Setting the matrix
#  2.Getting the matrix.
#  3.Setting the inverse.
#  4.Getting the inverse.
# The second function computes the inverse of the the matrix in the 'special' object.  However,
# if the inverse of this matrix is already in the cache, it will return the cached value 
# instead of performing the computation.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                   #initialize m
  set <- function(y){         #creates and stores the matrix that will be inverted.
    x <<- y           
    m <<- NULL
  }
  get <- function() x         #returns the set matrix.
  setinverse <- function(inverse) m <<- inverse     #Used by cachesolve to store the inverse once it has been found.
  getinverse <- function() m   #returns NULL if there is nothing stored and the inverse of the last matrix calculated otherwise.
  list(set = set, get = get,
       setinverse= setinverse,   #makeCacheMatrix returns a list containing the values of the functions contained.
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()             #assigns the value of m in makeCacheMatrix (either NULL or the inverse) to m in this function.
  if(!is.null(m)) {                #If the inverse has been previously stored
    message("getting cached data") 
    return(m)                       #return the stored inverted matrix.
  }
  data <- x$get()                   #assigns the matrix to the data variable using get() from makeCacheMatrix
  m <- solve(data, ...)             #Solves for the inverse matrix, stores it in the cache, 
  x$setinverse(m)                   #and also returns it.
  m
}
