##  R Programming Course
##  J. Loughner
##  22 June 2014

##  This program accepts a matrix and computers the inverse of that matrix and caches
##  the result.  If the matrix has not changed and the inverse is "requested" again,
##  then the return matrix comes from the cached inverse matrix.


##  This function creates a special vector that is a list containing functions to:
      ##  a.  Set the matrix
      ##  b.  Get the matrix
      ##  c.  Set the inverse of the matrix
      ##  d.  Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##  This function calculate the inverse of a matrix using the solve() function.
##  Before calculating the inverse, it checks to see if it was already calculated and
##  cached.  If so, then it takes the inverse from the cache.  If it was not already
##  cached, then it calculates the inverse and caches it for future use.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
            i <- x$getinverse()
            if(!is.null(i)) {
                  message("Using cached data for faster processing.")
                  return(i)
            }
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
            i
      }



## Test that the functions work:

      ## v <- makeCacheMatrix()
      ## v$set(rbind(c(4, 3),c(3, 2)))
      ## v$get()
      ## cacheSolve(v)
      ## cacheSolve(v)
      ## v$set(rbind(c(-2, 3),c(3, -4)))
      ## cacheSolve(v)
      ## cacheSolve(v)

