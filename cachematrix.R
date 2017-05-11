## by ISMAIL YILDIZ

makeCacheMatrix <- function(x = matrix()) {
     inverse_matrix <- NULL # define
     set <- function(y) {
          x <<- y
          inverse_matrix <<- NULL 
     }
     get <- function() x #get matrix
     setInverse <- function(solve) inverse_matrix <<- solve #set inverse matrix
     getInverse <- function() inverse_matrix #get inverse matrix
     list(
          set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse
     )  ## create list of functions for further use
}


cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse_matrix <- x$getInverse()                 #query the x matrix's cache
     if(!is.null(inverse_matrix)){                    #if there is a cache the inverse has been previously calculated
          message("getting cached data")    # sent message indicating this is just cache 
          return(inverse_matrix)                         # return the cache  
     }
     data <- x$get()                     # get the matrix used by makeCacheMatrix function 
     inverse_matrix <- solve(data, ...)               # calculate the inverse of the matrix
     x$setInverse(inverse_matrix)                     # store the inverse matrix in cache using the makeCacheMatrix set function
     inverse_matrix
}

