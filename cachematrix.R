makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      #Next part
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      
            #list the data
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      
      #Check IF Stmnt
      if(!is.null(m)) {
            message("getting cached data")
            
            return(m)
      }
      #use Data section
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
