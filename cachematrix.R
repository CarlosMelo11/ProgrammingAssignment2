#In this program there are two functions that help you to calculate the inverse of a matrix
#These functions called makeCacheMatrix and cacheSolve

## The first one, creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
    inver <- NULL                              
    set <- function(y) {                    
      x <<- y                             
      inver <<- NULL                  
    }
    get <- function() x                     
    setinverse <- function(inverse) inver <<- inverse  
    getinverse <- function() inver                    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
   }


## And the second one, computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
  
  cacheSolve <- function(x, ...) {
   
    inver <- x$getinverse()
    if(!is.null(inver)) {
      message("getting cached data")
      return(inver)
    }
    DATA <- x$get()
    inver <- solve(DATA, ...)
    x$setinverse(inver)
    inver
  }
