## There are 2 functions in this document -- makeCacheMatrix and cacheSolve
## Detailed descriptions are included below 

## makeCacheMatrix - creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## I used the following statement when I created test data and validated the functions work properly
  ## test_data <<- matrix(c(10,20,30,40), nrow=2, ncol=2, byrow = TRUE)
 
  ## Find out if the matrix and inverse variables are already cached (the objects already exists in memory)
  ## If either cached variable does not already exist in memory then set both to NULL to ensure clean start
  ## Note the <<- operator which makes the variables accessible by the lower level environment (called function)
  
  if (exists("cached_matrix") == 0 | exists("cached_inverse") == 0) {
      cached_matrix <<- NULL
      cached_inverse <<- NULL
  }
  
  ## Get the inverse of the matrix via cacheSolve
  ## cacheSolve returns the cached inverse if the new and old matrixes are the same
  ## Otherwise cacheSolve solves and returns the new inverse which will put into cache for re-use
  
  x_inverse <- cacheSolve(x)
  cached_matrix <<- x
  cached_inverse <<- x_inverse
  
  cached_inverse

}


## cacheSolve - computes the inverse matrix of the special "matrix" returned by makeCacheMatrix.
## If inverse already exists (and matrix hasn't changed), cacheSolve should retrieve
## the inverse matrix from the cache
## Otherwise it should solve for the new inverse matrix

cacheSolve <- function(x, ...) {
  
  ## Determine if matrix has changed - no need to recalc if not changed
  ## Cached matrix is called cached_matrix
  ## Cached inverse of matrix is called cached_inverse
  ## Both cached_matrix and cached_inverse are cached in memory by the calling function
  ## "x,y" in this function are local to this function
  
  matequal <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  }
    if (matequal(x, cached_matrix) == TRUE) {
      message("returning saved cached inverse of matrix")
      saved_inverse <<- cached_inverse
    }
  else {
    saved_inverse <<- solve(x)
  }
  saved_inverse
}
