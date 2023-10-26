## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function is to create a vector of 4 functions:
##  1. initializing matrix x and put i null;
##  2. output the x
##  3. calculate the inverse of matrix x
##  4. output the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## Write a short comment describing this function
##A vector value from the makeCacheMatrix() is required to input this function.
##It will first judge whether i, the inverse matrix, is NULL.
##If it's the first time run the function, i will be NULL, and then calculation will be done
##Else a message and a value from cache will be output.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data,...)
  x$setsolve(i)
  i
}
