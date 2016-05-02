## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object to cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  #Inverse matrix variable initialization
  i <- NULL
  # setting the value of x and reset inverse of matrix
  set <- function(y){
    x <<- y
    i <<- NULL 
  }
  #get the  matrix
  get <- function() x
  
  #setting the inverse of matrix
  setinverse <- function(inverse) i <<- inverse
  
  #getting the inverse of matrix
  getinverse <- function() i
  
  # Return a list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Function to compute the inverse using cache
cacheSolve <- function(x, ...) {
        #get the inverse 
        i <- x$getinverse()
        
        #if already exists, get from cache
        if(!is.null(i)){
          message("Getting cached data")
          return(i)
        }
        #getting the matrix data
        data <- x$get()
        
        #creating the inverse of matrix
        i <- solve(data, ...)
        #setting the inverse of matrix
        x$setinverse(i)
        i
}
