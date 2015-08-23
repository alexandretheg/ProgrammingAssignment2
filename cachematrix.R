##makeCacheMatrix creates a matrix object and sets its inverse
##It defines four functions and outputs them as a list:
##a function to set the matrix value, a function to fetch it, 
##and two similar functions for its inverse

makeCacheMatrix <- function(x=matrix()) {
  
  #this sets an object i to be null
  i <- NULL
  
  #this sets y to be equal to the matrix object x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #this sets 'get' equal to a function executing x (the matrix object) 
  get <- function()x
  
  #this sets 'setinverse' to a function assigning the argument 'solve' to the object i
  setinverse <- function(solve)i <<- solve
  
  #this sets 'getinverse' to a function executing i
  getinverse <- function()i
  
  #this creates a list with all four functions defined in makeCacheMatrix. This list is the output of the function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve is designed to take the list output by makeCacheMatrix as an argument.
##It takes the matrix makeCacheMatrix uses as an argument and takes it inverse.
##If it has already done this and the inverse exists, it fetches it and prints it
##without additional computing

cacheSolve <- function(x, ...) {
  
  #this sets i to the 'getinverse' function in the makeCacheMatrix list
  i <- x$getinverse()
  
  #this checks whether a value for i (the inverse of the matrix) already exists.
  #If it does the function prints a message, fetches the value, and prints it.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #this sets 'data' to the 'get' function in the list (e.g the matrix x)
  data <- x$get()
  
  #this sets i to the solve function which takes the inverse of 'data'
  i <- solve(data, ...)
  
  #this sets the 'setinverse' function from the list to i
  x$setinverse(i)
  i
}
