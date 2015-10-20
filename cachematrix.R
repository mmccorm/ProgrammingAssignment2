## cachematrix.R 102015 last modified 102015
# This is the programming assignment for week 3 of the coursera R programming course
# in the JHU data science specialization

## function makeCacheMatrix
# This function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
#
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## function cacheSolve
# The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache
# and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the
# cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# works identically to the example functions makeVector and cachemean as of 102015
# e.g. this console input/output once the example or new functions are entered
# for below:
# these are inputs
# # these are outputs
## these are comments to this bit

## Testing the example functions for cacheing mean of a vector
# y<-1:20
# madey<-makeVector(y)
# madey$get()
# # [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
# madey$getmean()
# #NULL
# cachemean(madey)
# # [1] 10.5
# madey$getmean()
# # [1] 10.5
# cachemean(madey)
# # getting cached data
# # [1] 10.5
# madey$setmean("Can set this to whatever.")
# madey$getmean()
# # [1] "Can set this to whatever."

## Now testing the new functions for cacheing the inverse of a matrix
# testmatrix<-matrix(c(2, 4, 3, 1, 5, 7,4,5,6), nrow=3, ncol=3) 
# testmatrix
# #      [,1] [,2] [,3]
# # [1,]    2    1    4
# # [2,]    4    5    5
# # [3,]    3    7    6
# madetest<-makeCacheMatrix(testmatrix)
# madetest$get()
# # [,1] [,2] [,3]
# # [1,]    2    1    4
# # [2,]    4    5    5
# # [3,]    3    7    6
# madetest$getinverse()
# # NULL
# cacheSolve(madetest)
# # [,1]          [,2]       [,3]
# # [1,] -0.1515152  6.666667e-01 -0.4545455
# # [2,] -0.2727273  3.416071e-17  0.1818182
# # [3,]  0.3939394 -3.333333e-01  0.1818182
# madetest$getinverse()
# # [,1]          [,2]       [,3]
# # [1,] -0.1515152  6.666667e-01 -0.4545455
# # [2,] -0.2727273  3.416071e-17  0.1818182
# # [3,]  0.3939394 -3.333333e-01  0.1818182
# cacheSolve(madetest)
# # getting cached data
# # [,1]          [,2]       [,3]
# # [1,] -0.1515152  6.666667e-01 -0.4545455
# # [2,] -0.2727273  3.416071e-17  0.1818182
# # [3,]  0.3939394 -3.333333e-01  0.1818182
# madetest$setinverse("Can set this also to whatever.")
# madetest$getinverse()
# # [1] "Can set this also to whatever."

## end 102015