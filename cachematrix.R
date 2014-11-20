## makeCacheMatrix function is used to create an object .This created object is later accessed by  
## using cacheSolve function .The  functions namely get, getinverse, setinverse
## are called by the cacheSolve function.

## makeCacheMatrix is passed a matrix which is used to create a object . This object stores two values 
## The original matrix value and the inverse matrix value which is initially set to 'NULL'.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve accesses the object by fetching the value of the function used to store the object 
## If the inverse is not yet calculated, cacheSolve calculates the inverse and stores it in the object 
## created by the call to the makeCacheMatrix  function and returns the inverse. Next time 
##since the inverse is stored it calls the inverse.

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
