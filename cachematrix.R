## 'makeCacheMatrix' creates a matrix that will cache its inverse and 'cacheSolve'retreives either the 
## cached Matrix inverse or computes the inverse 



#Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than computing it
#repeatedly (there are also alternatives to matrix inversion that we will
#not discuss here). Your assignment is to write a pair of functions that
#cache the inverse of a matrix.

#Write the following functions:
  
 # 1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
# 2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the `solve`
#function in R. For example, if `X` is a square invertible matrix, then
#`solve(X)` returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.

##'makeCacheMatrix' creates a matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    }


##'cacheSolve'retreives either the cached Matrix inverse or computes the inverse 

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) 
    {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

#this is a test below

#source("ProgrammingAssignment2/cachematrix.R")
 my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

 my_matrix$get()

 my_matrix$getmatrix()
 cacheSolve(my_matrix)

# Second example
 
 my_matrix2 <- makeCacheMatrix(matrix(2:5,2,2))
 
 my_matrix2$get()
my_matrix2$getmatrix()
  cacheSolve(my_matrix2)
   
   
 