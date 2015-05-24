## This code contains two functions that will allow the invert of a matrix
## to be computed and then held in Cashe for re-use.
##
## The purpose is that by re-using the cashed matrix it can save time
## re-computing the inverse.
##
## This is only applicable if the original matrix remains the same
## otherwise the inverse would need to be computed again.


## to run the code, create a matrix:
## k = matrix(c(1,2,3,4),nrow=2,ncol=2)

## cacheSolve(makeCacheMatrix(k))



## This function creates the object m that will be the cache of the matrix

makeCacheMatrix <- function(x = numeric()) {
  ## Initialise m
  m <- NULL
  
  ## SET
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## GET
  get <- function() x
  
  ## SET INVERSE
  setsolve <- function(solve) m <<- solve
  
  ## GET INVERSE
  getsolve <- function() m
  
  ## transfer
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}


## This function will use the above function to get the cached matrix

cacheSolve <- function(x, ...) {
  ## Get the inverse matrix from in case it has already be prepared
  m <- x$getsolve()
  
  ## Test to see if NULL - if not then it is good to return the cached matrix
  if(!is.null(m)){
    ## and say that the cashed matrix is being use because
    ## otherwise the user will not be able to tell the difference
    message("Getting Cached Data")
    return(m)
  }
  
  ## If m was NULL, then make m the inverse of the matrix provided
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  ## now return m
  m
}