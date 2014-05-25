#####this file contains all the explanations from the discussion forum which help us solve this problem



####make an example matrix, that I know the inverse of:

##http://www.mathsisfun.com/algebra/matrix-inverse.html

B = matrix(c(4, 2, 7, 6), nrow=2, ncol=2)

# > B
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6

###use the solve() function to calculate the inverse:

# > solve(B)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4


makeVector <- function(x = numeric()) {       ##this is a list!!
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                         ##sets "get" to be x (or the argument that was passed in makeVector)
#   
#   There are two other subfunctions in makeVector(). The first is is setmean(). If I were king I'd have named 
#   this savecache(), because that's all it does: it takes a vector passed into it and stores it in m, the cache. 
#   By that token, getmean() should be named getcache(), because all it does is return the cache.
  setmean <- function(mean) m <<- mean      ##the value of setmean (and thus also of m) will be set by a line in cachemean
  getmean <- function() m
##making a named list that stores all the stuff so that it can be accessed again
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {         ##here x is whatever was created above as makeVector, let's say a<-makeVector(1:7)
  mlocal <- x$getmean()
  if(!is.null(mlocal)) {
    message("getting cached data")
    return(mlocal)
  }
  data <- x$get()         ##data is now the object that was created in makeVector, ie. a
  mlocal <- mean(data, ...)
  x$setmean(mlocal)            ##the setmean function was defined in makeVecotr!! All it does is taking a vector passed into it and stores it in m 
   mlocal                      ##when I call cachemean(a), this is why i see the result
}


#####dissecting the funny parts: why to write x so weirdly

# > # x as a numeric vector
#   > x <- 1
# > environment(x)
# NULL
# > # x as a function defined in global environment
#   > x <- function() 1
# > x()
# [1] 1

# ##you guys are right about the argument of makeVector being stored in the variable x. The way I like to think 
# about it is that x exists in the context of makeVector (so we can see x as a "member" of any "instance" 
# of makeVector - in this example, a) but x isn't in the list that is actually returned by makeVector. This is 
# kind of like setting x to have a "private" status. If we changed the code in lines 11-13 of the original post 
# so that the list returned also contains x = x, we'd be able to access x directly (e.g. a$x), rather than only 
# through the function get.
# 
# In the above example we create two different list objects, a and b. When we create b it doesn't overwrite the value of x in a,
# which shows that x is not universal: it's scope is limited to the object that contains it. Thus, also the mean is preserved.

