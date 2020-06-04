## This code is attempting to "cache" the inverse of a matrix by using
## the "<<-" operator to push values to the global environment.

## The variable "x" represents the matrix data, "i" is the value of the inverse,
## initially set to "NULL".  The "y" variable will represent the new matrix data
## that could affect the value of "i".  We then define four operations--"get" extracts the 
## matrix data, "setinverse" solves for the inverse, "getinverse" calls the value of the inverse,
## and list assigns each of those operational names to a subset of a list.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(solve)i<<- solve
  getinverse <- function()i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## After running a matrix through the function "makeCacheMatrix", we plug it into "cacheSolve"
## to recalibrate the cached inverse value.  We set "i" equal to the "getinverse" category, and if
## it's not NULL, we simply repeate that stored value.  If it is NULL, then that means we entered
## in a new set of data (which replaced our cached value with NULL), so the rest of the commands
## extract the new data, solve for the inverse, and then cache the new inverse value to our "setinverse"
## subcategory.

cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("Retrieving cached matrix, master!")
    return(i)
  }
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}
