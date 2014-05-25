## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the inverse
# 4.  get the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(inverse) m <<- inverse
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)

}


##The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
       
  mlocal <- x$getcache()
  if(!is.null(mlocal)) {
    message("getting cached data")
    return(mlocal)
  }
  data <- x$get()
  mlocal <- solve(data, ...)
  x$setcache(mlocal)
  mlocal
}



###use the following matrices for trying everything out:
##C<-matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3)
# > C
# [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
# > solve(C)
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1

##B<-matrix(c(4,2,7,6), nrow=2, ncol=2)
# > solve(B)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
