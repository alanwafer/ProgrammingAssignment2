
##Creating a list to respectively set and get the values of a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinv <- function(inv) inverse <<- inv
      getinv <- function() inverse
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}



##Computes the Inverse of a matrix if it's value hasn't already been set in MakeCacheMatrix above.
##Using the setinv() function it stores the inverse matrix going forward
cacheSolve <- function(x, ...) {
      inverse <- x$getinv()
      if(!is.null(inverse)) {
            message("getting cached data.")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinv(inverse)
      inverse
}

##

