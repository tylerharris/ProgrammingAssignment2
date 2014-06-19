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
#This function stores a user defined matrix (using set()), which can then be retrieved using 
#its get() function.
#It also stores the value of the inverse matrix calculated by cacheSolve(using setinverse()),
# and this inverse matrix can be retrieved using getinverse().
#makeCacheMatrix returns a list with the value of all the functions it contains.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                 
  set <- function(y){     
    x <<- y           
    m <<- NULL
  }
  get <- function() x         
  setinverse <- function(inverse) m <<- inverse     
  getinverse <- function() m   
  list(set = set, get = get,
       setinverse= setinverse,   
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function takes the 'special' object created by makeCacheMatrix and returns its inverse.  
#If the inverse has been previously cached, the function returns the cached inverse matrix.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()             
  if(!is.null(m)) {               
    message("getting cached data") 
    return(m)                       
  }
  data <- x$get()                   
  m <- solve(data, ...)             
  x$setinverse(m)                 
  m
}
